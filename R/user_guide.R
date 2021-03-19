# TODO: Document URL (and path?) parameters
# TODO: Show return value description (and possibly type)
# TODO: Implement support for "any" type or multiple types
# TODO: Implement validation for optional arguments
# TODO: If an optional argument is not provided, its variable must still be created at runtime

#' @importFrom htmltools tags
create_user_guide <- function(path, pr) {
  cached_ui <- NULL

  function(req, res) {
    if (is.null(cached_ui)) {
      cached_ui <<- render_user_guide(path, pr)
    }

    cached_ui
  }
}

render_user_guide <- function(path, pr) {
  apiSpec <- pr$getApiSpec()

  title <- apiSpec$info$title
  version <- apiSpec$info$version
  # TODO: Allow markdown?
  desc <- apiSpec$info$description

  ui <- htmltools::tagList(
    tags$header(
      tags$h1(
        title,
        if (!is.null(version)) paste0("(v", version, ")")
      ),
      tags$div(class = "api-desc",
        desc
      )
    ),
    tags$main(
      tags$div(class = "routes",
        lapply(extract_route_info(path, pr), render_route_info)
      )
    )
  )

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
      div(class = "args",
        h4("Arguments"),
        render_args(route_info$arg_spec)
      )
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

  paste0(calling_func, "(",
    "\"", route_info$path, "\"",
    usage_args,
    ")")
}

render_args <- function(arg_spec) {
  htmltools::withTags(
    table(
      tr(
        th("Name"),
        th("Type"),
        th("Description")
      ),
      mapply(names(arg_spec), arg_spec, FUN = function(nm, spec) {
        tr(
          td(class = "arg-name", code(nm)),
          td(class = "arg-type", normalize_type_to_tableau(spec$type)),
          td(class = "arg-desc",
            if (spec$optional) em("(Optional)"),
            if (any(nzchar(spec$desc))) spec$desc
          )
        )
      }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    )
  )
}

extract_route_info <- function(path, pr) {
  results <- lapply(pr$endpoints, function(routes) {
    lapply(routes, function(route) {
      path <- route$path
      func <- route$getFunc()
      arg_spec <- attr(func, "tableau_arg_specs", exact = TRUE)
      return_spec <- attr(func, "tableau_return_spec", exact = TRUE)
      if (!is.null(arg_spec)) {
        list(comments = route$comments, path = path, arg_spec = arg_spec, return_spec = return_spec)
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
