# MUST HAVE
# TODO: URL param default values should be shown

# NICE TO HAVE
# TODO: URL param values should be coerced to type (if possible?)

library(fontawesome)

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

  if (!rlang::is_null(warnings)) {
    warnings <- markdown::markdownToHTML(text = warnings, fragment.only = TRUE)

    ui <- htmltools::tagList(
      tags$header(
        tags$div(
          class="nav",
          tags$div(
            class="main-menu",
            tags$ul(
              tags$li(
                tags$a(
                  href = "./",
                  tags$div(
                    class="menuitem",
                    fa("chart-area")
                  ),
                  tags$span(
                    class="nav-text",
                    "Call your analytics extension(s) from Tableau"
                  )
                )
              ),
              tags$li(
                tags$a(
                  href = "./setup",
                  tags$div(
                    class="menuitem",
                    fa("cogs")
                  ),
                  tags$span(
                    class="nav-text",
                    "Configure Tableau to use your analytics extension"
                  )
                )
              ),
              tags$li(
                tags$a(
                  href = "./__docs__/",
                  tags$div(
                    class="menuitem",
                    fa("info-circle")
                  ),
                  tags$span(
                    class="nav-text",
                    "View your extension's Open API documentation"
                  )
                )
              ),
              tags$li(
                tags$a(
                  href = "./help",
                  tags$div(
                    class="menuitem",
                    fa("question-circle")
                  ),
                  tags$span(
                    class="nav-text",
                    "Help with plumbertableau and RStudio Connect"
                  )
                )
              )
            )
          )
        ),
        tags$div(
          class="title_desc_container",
          tags$h1(
            class="padded-fully title",
            title,
            if (!is.null(version)) paste0("(v", version, ")")
          )
        )
      ),
      tags$main(
        tags$h3(
          class="warning",
          "Warning: The following item(s) need to be resolved before your API will be accessible from Tableau!"
        ),
        tags$div(
          class="padded-flat-top",
          htmltools::HTML(warnings)
        )
      )
    )
  } else {
    # Strip description of links to other pages
    desc <- markdown::markdownToHTML(text = strip_md_links(apiSpec$info$description),
                                     fragment.only = TRUE)
    ui <- htmltools::tagList(
      tags$header(
        tags$div(
          class="nav",
          tags$div(
            class="main-menu",
            tags$ul(
              tags$li(
                tags$a(
                  href = "./",
                  tags$div(
                    class="menuitem",
                    fa("chart-area")
                  ),
                  tags$span(
                    class="nav-text",
                    "Call your analytics extension(s) from Tableau"
                  )
                )
              ),
              tags$li(
                tags$a(
                  href = "./setup",
                  tags$div(
                    class="menuitem",
                    fa("cogs")
                  ),
                  tags$span(
                    class="nav-text",
                    "Configure Tableau to use your analytics extension"
                  )
                )
              ),
              tags$li(
                tags$a(
                  href = "./__docs__/",
                  tags$div(
                    class="menuitem",
                    fa("info-circle")
                  ),
                  tags$span(
                    class="nav-text",
                    "View your extension's Open API documentation"
                  )
                )
              ),
              tags$li(
                tags$a(
                  href = "./help",
                  tags$div(
                    class="menuitem",
                    fa("question-circle")
                  ),
                  tags$span(
                    class="nav-text",
                    "Help with plumbertableau and RStudio Connect"
                  )
                )
              )
            )
          )
        ),
        tags$div(
          class="title_desc_container",
          tags$h1(
            class="padded-fully title",
            title,
            if (!is.null(version)) paste0("(v", version, ")")
          ),
          tags$div(class = "api-desc",
            tags$div(
              class="padded-flat-top",
              htmltools::HTML(desc)
            ),
          )
        )
      ),
      tags$main(
        tags$h3(
          class="subtitle",
          "Call your analytics extension(s) from Tableau"
        ),
        tags$div(class = "padded-fully routes",
                 lapply(extract_route_info(pr, path), render_route_info)
        )
      )
    )
  }

  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    content = ui
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
  desc <- ""
  if (rlang::is_null(warnings)) {
    desc <- markdown::markdownToHTML(text = strip_md_links(apiSpec$info$description),
                                   fragment.only = TRUE)
  }
  title <- apiSpec$info$title
  version <- apiSpec$info$version

  ui <- htmltools::tagList(
    tags$header(
      tags$div(
        class="nav",
        tags$div(
          class="main-menu",
          tags$ul(
            tags$li(
              tags$a(
                href = "./",
                tags$div(
                  class="menuitem",
                  fa("chart-area")
                ),
                tags$span(
                  class="nav-text",
                  "Call your analytics extension(s) from Tableau"
                )
              )
            ),
            tags$li(
              tags$a(
                href = "./setup",
                tags$div(
                  class="menuitem",
                  fa("cogs")
                ),
                tags$span(
                  class="nav-text",
                  "Configure Tableau to use your analytics extension"
                )
              )
            ),
            tags$li(
              tags$a(
                href = "./__docs__/",
                tags$div(
                  class="menuitem",
                  fa("info-circle")
                ),
                tags$span(
                  class="nav-text",
                  "View your extension's Open API documentation"
                )
              )
            ),
            tags$li(
              tags$a(
                href = "./help",
                tags$div(
                  class="menuitem",
                  fa("question-circle")
                ),
                tags$span(
                  class="nav-text",
                  "Help with plumbertableau and RStudio Connect"
                )
              )
            )
          )
        )
      ),
      tags$div(
        class="title_desc_container",
        tags$h1(
          class="padded-fully title",
          title,
          if (!is.null(version)) paste0("(v", version, ")")
        ),
        tags$div(class = "api-desc",
          tags$div(
            class="padded-flat-top",
            htmltools::HTML(desc)
          ),
        )
      )
    ),
    tags$main(
      tags$h3(
        class="subtitle",
        "Configure Tableau to access your extension"
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
  )

  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    content = ui
  ))
}

strip_md_links <- function(text) {
  i <- stringi::stri_locate_first(text, fixed = "#### Use the following links to setup and use your Tableau Analytics Extension.")
  substr(text, start=1, stop=i-1)
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
  warnings <- warning_message()

  desc <- ""
  if (rlang::is_null(warnings)) {
    desc <- markdown::markdownToHTML(text = strip_md_links(apiSpec$info$description),
                                   fragment.only = TRUE)
  }
  ui <- htmltools::tagList(
    tags$header(
      # htmltools::HTML(menu_html(), fragment.only = TRUE),
      htmltools::HTML("<h3>HI Bill!</h3>", fragment.only = TRUE),
      tags$div(
        class="title_desc_container",
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
    ),
    tags$main(
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
  )
  
  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    content = ui
  ))
}

help_text <- function() {
"
The plumbertableau package makes it simple for R developers to build [Tableau Analytics Extensions](https://tableau.github.io/analytics-extensions-api/) by lightly annotating Plumber APIs and deploy those extensions to [RStudio Connect](https://www.rstudio.com/products/connect/). It also makes it easy for Tableau developers to use those extensions, by automatically generating instructions and code examples tailored for Tableau.

### R and Tableau
Tableau is an industry leading visual analytics platform, used by many organizations to investigate, understand, and report on data. While Tableau offers some built-in analytics capabilities, it's often useful to run additional analytics from outside of Tableau, such as real time output from a predictive model in R. This type of real time reporting can be accomplished using Tableau Analytics Extensions^[See [Announcing the Analytics Extensions API](https://www.tableau.com/about/blog/2020/3/announcing-analytics-extensions-api)].

R is a popular programming language for statistics and data analysis. Extending Tableau workbooks with plumbertableau extensions lets Tableau developers leverage R's flexibility, power, and deep library of analytical packages. In this way, R and Tableau can be used to complement each other's strengths and provide more comprehensive data insights to decisionmakers.

### Implementation
The plumbertableau package is built on top of [Plumber](https://www.rplumber.io/), a package for developing APIs in R. plumbertableau extends Plumber to comply with the [Tableau Analytics API specification](https://tableau.github.io/analytics-extensions-api/docs/ae_api_ref.html). Developing an Analytics Extension for Tableau in R is, at its core, a matter of building a Plumber API, and annotating it with the extra information it needs to communicate with Tableau.

A simple example is creating an API that capitalizes all incoming text:

```{r capitalize, eval = FALSE}
```

Once this API has been developed, it can be [published to RStudio Connect](https://docs.rstudio.com/connect/user/publishing/) and used from within a Tableau workbook, running in either Tableau Desktop, Tableau Server, or Tableau Online. Extensions are accessed from calculated fields in Tableau. To use our example `capitalize` extension, we type the following into a Tableau calculated field:

```
SCRIPT_STR(\"/extension/path/capitalize\", \"Hello World\")
```

The first argumenent to the calculated field is the path to the API endpoint we wish to call. If the extension was published to RStudio Connect, this is path to the API on the server, `/extension/path\"`, plus the path to the specific endpoint, `/capitalize`.

The second (and subsequent) argument(s) are the values we want to pass from Tableau to the analytics extension. In this example, we're passing the string (`\"Hello World\"`), but this field can refer to data fields in the Tableau workbook.

### Learn more
* [R developer guide for getting started with developing Tableau extensions using plumbertableau](r-developer-guide.html)
* [Tableau developer guide for getting started using Tableau extensions built with plumbertableau](tableau-developer-guide.html)
* [Setting up Tableau for use with extensions hosted on RStudio Connect](tableau-configutation.html)

"
}

menu_html <- function() {
  htmltools::tagList(
    tags$div(
      class="nav",
      tags$div(
        class="main-menu",
        tags$ul(
          tags$li(
            tags$a(
              href = "./",
              tags$div(
                class="menuitem",
                fa("chart-area")
              ),
              tags$span(
                class="nav-text",
                "Call your analytics extension(s) from Tableau"
              )
            )
          ),
          tags$li(
            tags$a(
              href = "./setup",
              tags$div(
                class="menuitem",
                fa("cogs")
              ),
              tags$span(
                class="nav-text",
                "Configure Tableau to use your analytics extension"
              )
            )
          ),
          tags$li(
            tags$a(
              href = "./__docs__/",
              tags$div(
                class="menuitem",
                fa("info-circle")
              ),
              tags$span(
                class="nav-text",
                "View your extension's Open API documentation"
              )
            )
          ),
          tags$li(
            tags$a(
              href = "./help",
              tags$div(
                class="menuitem",
                fa("question-circle")
              ),
              tags$span(
                class="nav-text",
                "Help with plumbertableau and RStudio Connect"
              )
            )
          )
        )
      )
    )
  )
}
