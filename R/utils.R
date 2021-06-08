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
        call. = FALSE, immediate. = TRUE)
    }

    # Check for default (JSON) parser
    if (!is.null(route$parsers)) {
      warning(
        paste0("Route ",
               route$path,
               " includes a user specified parser. plumbertableau automatically sets the appropriate parser for Tableau requests. There is no need to specify a parser."),
        call. = FALSE, immediate. = TRUE)
    }

    # Check for default (JSON) serializer
    if (!is.null(route$serializer)) {
      warning(
        paste0("Route ",
               route$path,
               " includes a user specified serializer. plumbertableau automatically sets the appropriate serializer for Tableau requests. There is no need to specify a serializer."),
        call. = FALSE)
    }
  }
}

write_log_message <- function(req, res, msg = NULL) {
  # Desired behavior:
  # - Include Correlation ID in every log entry
  # - Only log the request body once for each request
  log_msg <- paste0(
    "[",
    Sys.time(),
    "] ",
    ifelse(rlang::is_null(req$HTTP_X_CORRELATION_ID), "", paste0("(", req$HTTP_X_CORRELATION_ID, ") ")),
    req$REQUEST_METHOD,
    " ",
    req$PATH_INFO,
    ifelse(rlang::is_null(msg), "", paste0(" - ", msg)),
    ifelse(rlang::is_null(req$body_log) && req$postBody != "", paste0(" - ", req$postBody), "")
  )

  req$body_log <- TRUE

  log_msg
}
