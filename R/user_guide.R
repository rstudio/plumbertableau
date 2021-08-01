# MUST HAVE
# TODO: URL param default values should be shown

# NICE TO HAVE
# TODO: URL param values should be coerced to type (if possible?)

library(fontawesome)

globalVariables(names(htmltools::tags))

# Create a Font Awesome `html_dependency`
fa_html_dependency()


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
          class="title_desc_container",
          tags$h1(
            class="padded-fully title",
            title,
            if (!is.null(version)) paste0("(v", version, ")")
          ),
        ),
        tags$div(
          class = "warning",
          tags$h4(
            "Warning: The following item(s) need to be resolved before your API will be accessible from Tableau!"
          )
        )
      ),
      tags$main(
        tags$div(
          class="padded-flat-top",
          htmltools::HTML(warnings)
        ),
        tags$div(class = "api-desc padded-flat-top",
          tags$a(href = "./__docs__/",
                class="button",
                "View your extension's Open API documentation"
          )
        )
      )
    )
  } else {
    # apiSpec <- pr$getApiSpec()

    # title <- apiSpec$info$title
    # version <- apiSpec$info$version

    # Strip description of links to other pages
    desc <- markdown::markdownToHTML(text = strip_md_links(apiSpec$info$description),
                                     fragment.only = TRUE)

    ui <- htmltools::tagList(
      tags$header(
        tags$div(
          class="title_desc_container",
          tags$h1(
            class="padded-fully title",
            title,
            if (!is.null(version)) paste0("(v", version, ")")
          ),
          tags$div(
            tags$div(
              class="padded-flat-top",
              htmltools::HTML(desc)
            ),
          ),
        ),
        tags$div(
          class="nav_div",
          tags$a(
            href = "./setup",
            class="button",
            "Configure Tableau to access your extension"
          ),
          tags$a(
            href = "./__docs__/",
            class="button",
            "View your extension's Open API documentation"
          )
        )
      ),
      tags$main(
        tags$h3(
          class="subtitle",
          "Use your analytics extension from Tableau"
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

  title <- apiSpec$info$title
  version <- apiSpec$info$version

  ui <- htmltools::tagList(
    fa_html_dependency(),
    tags$header(
      htmltools::HTML(menu_html()),
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
        ),
      ),
      tags$div(
        class="nav_div",
        tags$a(
          href = "./",
          class="button",
          "Use your analytics extension from Tableau"
        ),
        tags$a(
          href = "./__docs__/",
          class="button",
          "View your extension's Open API documentation"
        )
      ),
    ),
    tags$main(
      tags$h3(
        class="subtitle",
        "Configure Tableau to access your extension"
      ),
      tags$div(
        class="padded-flat-top",
        tags$p(fa("car", fill = "purple"), "Manufacturer:"),
        tags$h4("If you are using Tableau Server or Tableau Online:"),
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
        tags$h4("If you are using Tableau Desktop:"),
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

menu_html <- function() {
# <div class="area"></div>
'  <nav class="main-menu">
    <ul>
      <li>
        <a href="http://justinfarrow.com">
          <i class="fa fa-home fa-2x"></i>
          <span class="nav-text">
              Dashboard
          </span>
        </a>
      </li>
      <li class="has-subnav">
        <a href="#">
          <i class="fa fa-laptop fa-2x"></i>
          <span class="nav-text">
              Stars Components
          </span>
        </a>
      </li>
      <li class="has-subnav">
        <a href="#">
          <i class="fa fa-list fa-2x"></i>
          <span class="nav-text">
              Forms
          </span>
        </a>
      </li>
      <li class="has-subnav">
        <a href="#">
          <i class="fa fa-folder-open fa-2x"></i>
          <span class="nav-text">
              Pages
          </span>
        </a>
      </li>
      <li>
        <a href="#">
          <i class="fa fa-bar-chart-o fa-2x"></i>
          <span class="nav-text">
              Graphs and Statistics
          </span>
        </a>
      </li>
      <li>
        <a href="#">
          <i class="fa fa-font fa-2x"></i>
          <span class="nav-text">
              Quotes
          </span>
        </a>
      </li>
      <li>
        <a href="#">
          <i class="fa fa-table fa-2x"></i>
          <span class="nav-text">
              Tables
          </span>
        </a>
      </li>
      <li>
        <a href="#">
          <i class="fa fa-map-marker fa-2x"></i>
          <span class="nav-text">
              Maps
          </span>
        </a>
      </li>
      <li>
        <a href="#">
          <i class="fa fa-info fa-2x"></i>
          <span class="nav-text">
              Documentation
          </span>
        </a>
      </li>
    </ul>

    <ul class="logout">
      <li>
        <a href="#">
          <i class="fa fa-power-off fa-2x"></i>
          <span class="nav-text">
              Logout
          </span>
        </a>
      </li>  
    </ul>
  </nav>
  '
}
