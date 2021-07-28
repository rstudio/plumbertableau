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

  if (!rlang::is_null(warnings)) {
    warnings <- markdown::markdownToHTML(text = warnings, fragment.only = TRUE)
    ui <- htmltools::tagList(
      tags$header(
        class = "warning",
        tags$h1(
          "Warning"
        ),
        tags$h3(
          "The following item(s) need to be resolved before your API will be accessible from Tableau"
        )),
      tags$main(
        htmltools::HTML(warnings),
        tags$a(href = "./__docs__/",
               "Open API documentation")
      )
    )
  } else {
    apiSpec <- pr$getApiSpec()

    title <- apiSpec$info$title
    version <- apiSpec$info$version
    # Strip description of links to other pages
    desc <- markdown::markdownToHTML(text = strip_md_links(apiSpec$info$description),
                                     fragment.only = TRUE)

    ui <- htmltools::tagList(
      tags$header(
        tags$h1(
          title,
          if (!is.null(version)) paste0("(v", version, ")")
        ),
        tags$div(class = "api-desc",
                 htmltools::HTML(desc),
                 tags$p(
                   tags$a(
                     href = "./setup",
                     "Tableau Setup Instructions"
                   ),
                   tags$br(),
                   tags$a(
                     href = "./__docs__/",
                     "Open API Documentation"
                   )
                 )
        )
      ),
      tags$main(
        tags$div(class = "routes",
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
  desc <- markdown::markdownToHTML(text = strip_md_links(apiSpec$info$description),
                                   fragment.only = TRUE)

  ui <- htmltools::tagList(
    tags$header(
      tags$h1(
        "Tableau Setup"
      ),
      tags$div(class = "api-desc",
               htmltools::HTML(desc),
               tags$p(
                 tags$a(
                   href = "./",
                   "Tableau Usage Instructions"
                 ),
                 tags$br(),
                 tags$a(
                   href = "./__docs__/",
                   "Open API Documentation"
                 )
               )
      )
    ),
    tags$main(
      htmltools::HTML(
        markdown::markdownToHTML(
          text = glue::glue("#### Tableau Server / Tableau Online
  1. Using an administrative account, login to Tableau Server/Online
  2. Navigate to Settings, then Extensions
  3. Under the heading 'Analytics Extensions', select 'Enable analytics extension for site'
  4. Create a new connection and select the connection type of 'Analytics Extensions API'
  5. Select if you want to use SSL and enter the server Host (`{server_domain}`) and Port (`{server_port}`) for your RStudio Connect server
  6. Select 'Sign in with a username and password'. The username is 'rstudio-connect' and the password is any valid API key from RStudio Connect
  8. Create / Save changes

#### Tableau Desktop
  1. Navigate to Help, Settings and Performance, Manage Analytics Extension Connection...
  2. Select 'TabPy/External API'
  3. Set Server (`{server_domain}`) and Port (`{server_port}`) to the address and port of the server running the API
  4. If desired, select 'Sign in with a username and password'. The username is 'rstudio-connect' and the password is any valid API key from RStudio Connect
  5. Select whether to Require SSL
  6. Save changes"),
fragment.only = TRUE
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
  stringi::stri_replace_all(text, regex = " +\\*.*", "")
}
