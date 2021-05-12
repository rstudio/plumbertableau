#' Check Plumber route for Tableau compliance
#'
#' @param route A plumber route
#'
#' @return Provides warnings based on features of \code{route}
check_route <- function(route) {
  # Recursively work through mounted / nested routes
  if ("Plumber" %in% class(route)) {
    lapply(route$routes, check_route)
  } else if (is.list(route)) {
    lapply(route, check_route)
  } else {
    # Check for POST endpoints
    if (!("POST" %in% route$verbs)) {
      warning(
        paste0("Tableau endpoints must accept POST requests. ",
               route$path,
               " does not respond to POST requests."),
        call. = FALSE)
    }

    # Check for JSON parser
    if (!("json" %in% names(route$parsers$alias))) {
      warning(
        paste0("Tableau submits JSON requests. ",
               route$path,
               " does not include a JSON parser."),
        call. = FALSE)
    }
  }
}
