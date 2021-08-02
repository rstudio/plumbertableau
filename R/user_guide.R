# MUST HAVE
# TODO: URL param default values should be shown

# NICE TO HAVE
# TODO: URL param values should be coerced to type (if possible?)

globalVariables(names(htmltools::tags))

#' @importFrom htmltools tags
create_user_guide <- function(pr) {
  function(req, res) {
    "!DEBUG `write_log_message(req, res, 'Generating Tableau Usage Instructions')"
    # Parse the endpoint path from header on RStudio Connect
    content_path <- NULL
    if (!rlang::is_null(req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL)) {
      base_url <- req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL
      rsc_root <- Sys.getenv("CONNECT_SERVER")
      content_path <- gsub(rsc_root,
                           "",
                           base_url,
                           fixed = TRUE
      )

      # If the path requested is not root (/), strip it from the content path
      if (req$PATH_INFO != "/") {
        content_path <- gsub(req$PATH_INFO,
                             "",
                             content_path,
                             fixed = TRUE
        )
      }

      # Ensure path starts with '/'
      if (!stringi::stri_startswith(content_path, fixed = "/")) content_path <- paste0("/", content_path)
    }
    render_user_guide(content_path, pr)
  }
}

render_user_guide <- function(path, pr) {
  warnings <- warning_message()

  apiSpec <- pr$getApiSpec()
  title <- apiSpec$info$title
  version <- apiSpec$info$version
  desc <- markdown::markdownToHTML(text = apiSpec$info$user_description,
                                    fragment.only = TRUE)
  body_content <- "body_content not generated"

  title_desc <- htmltools::tagList(
    tags$h1(
      class="padded-fully title",
      title,
      if (!is.null(version)) paste0("(v", version, ")")
    ),
    tags$div(class = "api-desc",
      tags$div(
        class="padded-flat-top",
        htmltools::HTML(desc)
      )
    )
  )

  if (!rlang::is_null(warnings)) {
    body_content <- htmltools::tagList(
      tags$h3(
        class="warning",
        "Warning: The following item(s) need to be resolved before your API will be accessible from Tableau."
      ),
      tags$div(
        class="padded-flat-top",
        htmltools::HTML(markdown::markdownToHTML(text = warnings, fragment.only = TRUE))
      )
    )
  } else {
    body_content <- htmltools::tagList(
      tags$h3(
        class="subtitle",
        "Use this extension in Tableau"
      ),
      tags$div(class = "padded-fully routes",
        lapply(extract_route_info(pr, path), render_route_info)
      )
    )
  }

  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    title_desc = title_desc,
    body_content = body_content
  ))
}

render_route_info <- function(route_info) {
  htmltools::withTags(
    div(class = "route",
      h3(id = route_info$path, class = "path",
        route_info$path,
        a(class = "permalink", href = paste0("#", route_info$path),
          "#"
        )
      ),
      if (any(nzchar(route_info$comments))) {
        div(class = "desc",
          h4("Description"),
          div(route_info$comments)
        )
      },
      div(class = "usage",
        h4("Usage"),
        code(render_usage(route_info))
      ),
      if (length(route_info$param_spec) > 0) {
        div(class = "params",
          h4("Params"),
          render_args(route_info$param_spec)
        )
      },
      if (length(route_info$arg_spec) > 0) {
        div(class = "args",
          h4("Arguments"),
          render_args(route_info$arg_spec)
        )
      },
      if (length(route_info$return_spec$desc) > 0 && any(nzchar(route_info$return_spec$desc))) {
        div(class = "return",
          h4("Return value"),
          table(class = "items",
            tr(
              th("Type"),
              th("Description")
            ),
            tr(
              td(route_info$return_spec$type),
              td(route_info$return_spec$desc)
            )
          )
        )
      }
    )
  )
}

render_usage <- function(route_info) {
  calling_func <- switch(route_info$return_spec$type,
    "logical" = "SCRIPT_BOOL",
    "character" = "SCRIPT_STR",
    "numeric" = "SCRIPT_REAL",
    "integer" = "SCRIPT_INT"
  )

  usage_args <- if (length(route_info$arg_spec) > 0) {
    paste0(", ", names(route_info$arg_spec), collapse = "")
  }

  query <- ""
  if (length(route_info$param_spec) > 0) {
    query <- paste0("?",
      paste(collapse = "&",
        paste0(names(route_info$param_spec), "=<", names(route_info$param_spec), ">")
      )
    )
  }

  paste0(calling_func, "(",
    "\"", route_info$path, query, "\"",
    usage_args,
    ")")
}

render_args <- function(arg_spec) {
  htmltools::withTags(
    table(class = "items",
      tr(
        th("Name"),
        th("Type"),
        th("Description")
      ),
      mapply(names(arg_spec), arg_spec, FUN = function(nm, spec) {
        tr(
          td(class = "item-name", code(nm)),
          td(class = "item-type", normalize_type_to_tableau(spec$type)),
          td(class = "item-desc",
            if (spec$optional) em("(Optional)"),
            if (any(nzchar(spec$desc))) spec$desc
          )
        )
      }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    )
  )
}

extract_route_info <- function(pr, path = NULL) {
  # Returns a list containing a bunch of attributes about each route.
  results <- lapply(pr$endpoints, function(routes) {
    lapply(routes, function(route) {
      if (rlang::is_null(path)) {
        path <- route$path
      } else {
        path <- ifelse(grepl(paste0("^", path), route$path), route$path, paste0(path, route$path))
        # Remove any potential double // from path
        path <- gsub("//", "/", path)
      }

      func <- route$getFunc()
      arg_spec <- attr(func, "tableau_arg_specs", exact = TRUE)
      return_spec <- attr(func, "tableau_return_spec", exact = TRUE)
      param_spec <- extract_param_spec(route)
      if (!is.null(arg_spec)) {
        list(comments = route$comments, path = path, param_spec = param_spec,
          arg_spec = arg_spec, return_spec = return_spec)
      } else {
        NULL
      }
    })
  })
  results <- unname(results)
  results <- unlist(recursive = FALSE, results)
  results <- results[vapply(results, Negate(is.null), logical(1))]
  results
}

extract_param_spec <- function(route) {
  params <- route$getEndpointParams()
  func <- route$getFunc()
  defaults <- lapply(as.list(formals(func)), function(x) {
    if (is.character(x) || is.numeric(x) || is.integer(x)) {
      x
    } else if (is.logical(x)) {
      x
    } else {
      NULL
    }
  })

  mapply(names(params), params, FUN = function(nm, param) {
    param_spec(type = param$type,
      desc = param$desc,
      optional = !isTRUE(param$required),
      default = defaults[[nm]]
    )
  }, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

create_setup_instructions <- function(pr) {
  cached_instructions <- NULL

  function(req, res) {
    "!DEBUG `write_log_message(req, res, 'Generating Tableau Setup Instructions')"
    if (is.null(cached_instructions)) {
      # Caching works b/c R is restarted when the vanity path changes on RStudio Connect
      cached_instructions <<- render_setup_instructions(req$content_path, pr)
    }

    cached_instructions
  }
}

render_setup_instructions <- function(path, pr) {
  connect_server <- Sys.getenv("CONNECT_SERVER")
  server_domain <- urltools::domain(connect_server)
  server_port <- urltools::port(connect_server)
  warnings <- warning_message()

  if (rlang::is_na(server_port)) {
    server_scheme <- urltools::scheme(connect_server)
    if (rlang::is_na(server_scheme)) {
      # Do nothing
    } else if (server_scheme == "http") {
      server_port <- 80
    } else if (server_scheme == "https")
      server_port <- 443
  }
  apiSpec <- pr$getApiSpec()
  desc <- markdown::markdownToHTML(text = apiSpec$info$user_description,
                                    fragment.only = TRUE)
  title <- apiSpec$info$title
  version <- apiSpec$info$version
  body_content <- "body_content not generated"

  title_desc <- htmltools::tagList(
    tags$h1(
      class="padded-fully title",
      title,
      if (!is.null(version)) paste0("(v", version, ")")
    ),
    tags$div(class = "api-desc",
      tags$div(
        class="padded-flat-top",
        htmltools::HTML(desc)
      )
    )
  )

  if (!rlang::is_null(warnings)) {
    body_content <- htmltools::tagList(
      tags$h3(
        class="warning",
        "Warning: The following item(s) need to be resolved before your API will be accessible from Tableau!"
      ),
      tags$div(
        class="padded-flat-top",
        htmltools::HTML(markdown::markdownToHTML(text = warnings, fragment.only = TRUE))
      )
    )
  } else {
    body_content <- htmltools::tagList(
      tags$h3(
        class="subtitle",
        "Configure Tableau to access this extension"
      ),
      tags$div(
        class="padded-flat-top",
        tags$h3("If you are using Tableau Server or Tableau Online:"),
        tags$ol(
          tags$li("Using an administrative account, login to Tableau Server/Online"),
          tags$li("Navigate to Settings, then Extensions"),
          tags$li("Under the heading 'Analytics Extensions', select 'Enable analytics extension for site"),
          tags$li("Create a new connection and select the connection type of 'Analytics Extensions API'"),
          tags$li("Select if you want to use SSL"),
          tags$li("Enter the information for your RStudio Connect Server:"),
          tags$div(
            class="values",
            tags$div(
              tags$span(class="emphasized", "Host:"),
              server_domain
            ),
            tags$div(
              tags$span(class="emphasized", "Port:"),
              server_port
            )
          ),
          tags$li("Select 'Sign in with a username and password' and enter the credentials:"),
          tags$div(
            class="values",
            tags$div(
              tags$span(class="emphasized", "Username:"),
              "rstudio-connect"
            ),
            tags$div(
              tags$span(class="emphasized", "Password:"),
              tags$span(class="italic", "any valid API key from RStudio Connect")
            )
          ),
          tags$li("Create / Save changes")
        )
      ),
      tags$div(
        class="padded-flat-top",
        tags$h3("If you are using Tableau Desktop:"),
        tags$ol(
          tags$li("Navigate to Help, Settings and Performance, Manage Analytics Extension Connection..."),
          tags$li("Select 'TabPy/External API'"),
          tags$li("Enter the information for your RStudio Connect Server:"),
          tags$div(
            class="values",
            tags$div(
              tags$span(class="emphasized", "Host:"),
              server_domain
            ),
            tags$div(
              tags$span(class="emphasized", "Port:"),
              server_port
            )
          ),
          tags$li("If desired, select 'Sign in with a username and password' and enter the credentials:"),
          tags$div(
            class="values",
            tags$div(
              tags$span(class="emphasized", "Username:"),
              "rstudio-connect"
            ),
            tags$div(
              tags$span(class="emphasized", "Password:"),
              tags$span(class="italic", "any valid API key from RStudio Connect")
            )
          ),
          tags$li("Select whether to Require SSL"),
          tags$li("Save changes")
        )
      )
    )
  }

  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    title_desc = title_desc,
    body_content = body_content
  ))
}

#' @importFrom htmltools tags
create_help <- function(pr) {
  function(req, res) {
    "!DEBUG `write_log_message(req, res, 'Generating Tableau Analytical Extension Overview')"
    # Parse the endpoint path from header on RStudio Connect
    content_path <- NULL
    if (!rlang::is_null(req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL)) {
      base_url <- req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL
      rsc_root <- Sys.getenv("CONNECT_SERVER")
      content_path <- gsub(rsc_root,
                           "",
                           base_url,
                           fixed = TRUE
      )

      # If the path requested is not root (/), strip it from the content path
      if (req$PATH_INFO != "/") {
        content_path <- gsub(req$PATH_INFO,
                             "",
                             content_path,
                             fixed = TRUE
        )
      }

      # Ensure path starts with '/'
      if (!stringi::stri_startswith(content_path, fixed = "/")) content_path <- paste0("/", content_path)
    }
    render_help(content_path, pr)
  }
}

render_help <- function(path, pr) {
  apiSpec <- pr$getApiSpec()
  title <- apiSpec$info$title
  version <- apiSpec$info$version
  desc <- markdown::markdownToHTML(text = apiSpec$info$user_description,
                                    fragment.only = TRUE)

  title_desc <- htmltools::tagList(
    tags$h1(
      class="padded-fully title",
      title,
      if (!is.null(version)) paste0("(v", version, ")")
    ),
    tags$div(class = "api-desc",
      tags$div(
        class="padded-flat-top",
        htmltools::HTML(desc)
      )
    )
  )

  body_content <- htmltools::tagList(
    tags$h3(
      class="subtitle",
      "Introduction to plumbertableau and RStudio Connect"
    ),
    tags$div(
      class = "padded-fully",
      htmltools::HTML(markdown::markdownToHTML(
        text = help_text()), fragment.only = TRUE
      )
    )
  )
  
  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    title_desc = title_desc,
    body_content = body_content
  ))
}

help_text <- function() {
"
---
title: \"Introduction\"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = \"#>\"
)

knitr::read_chunk(path = \"../inst/plumber/capitalize/plumber.R\",
                  from = 9,
                  labels = \"capitalize\")
```

[Tableau](https://www.tableau.com) is a leading visual analytics platform that lets its users investigate, understand, and report on data. R is a programming language for statistics, data analysis, and visualization.

plumbertableau lets you call external R code from Tableau workbooks via [Tableau Analytics Extensions](https://tableau.github.io/analytics-extensions-api/). You achieve this by writing a plumbertableau extension, which is a [Plumber](https://www.rplumber.io/) API with some extra annotations. plumbertableau uses these annotations to correctly serve requests from Tableau, as well as dynamically generate documentation, copy-and-pasteable Tableau code, and setup instructions.

plumbertableau extensions are most easily used with [RStudio Connect](https://www.rstudio.com/products/connect/), which lets you host and manage any number of Tableau extensions along with other content types.

## A Simple Example

The following code is a simple plumbertableau extension. It can receive text from Tableau and returns that text capitalized.

```{r capitalize, eval = FALSE}
```

The core of this extension is a very simple R function that capitalizes text. It's surrounded by Plumber \"annotations\" which describe the web service. plumbertableau introduces new annotations in addition to what Plumber already provides. To learn or more information on these, see the guide to **[Writing plumbertableau Extensions in R](r-developer-guide.html)**.

Before you use the extension in Tableau, Tableau needs to be able to access it. You can find instructions on how to do that, and information on publishing extensions to RStudio Connect, in **[Publishing plumbertableau Extensions to RStudio Connect](publishing-extensions.html)**. This document will walk you through publishing extensions to RStudio Connect and setting up a Connect server as an extension in Tableau.

plumbertableau extensions are used in Tableau's *calculated fields*. Let's imagine we've published our extension to RStudio Connect and have given it the custom URL `stringutils`. To use our `capitalize` extension, we'd type the following into a Tableau calculated field, or just copy and paste it from the automatically generated code samples. (In real usage, you'll probably replace `\"Hello World\"` with references to Tableau data.)

```
SCRIPT_STR(\"/stringutils/capitalize\", \"Hello World\")
```

Tableau will send a request to the server you configured, and the server will send it on to the extension you named in the first argument (in this case, `/stringutils/capitalize`). All the other arguments are data sent to the extension. For more information on using published extensions in Tableau, see the guide to **[Using plumbertableau Extensions in Tableau](tableau-developer-guide.html)**.

## Learn More

- [Writing plumbertableau Extensions in R](r-developer-guide.html)
- [Publishing plumbertableau Extensions to RStudio Connect](tableau-configutation.html)
- [Using plumbertableau Extensions in Tableau](tableau-developer-guide.html)

"
}
