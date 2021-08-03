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
