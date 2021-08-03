#' Checks a Plumber route for Tableau compliance
#'
#' Checks a route to ensure that it accepts POST requests and uses the default JSON parser and serializer.
#'
#' @param route A plumber route
#'
#' @return Provides warnings based on features of \code{route}
#' @keywords internal
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


# Utilities for capturing endpoint execution time
preroute_hook <- function(data, req, res) {
  # Capture execution start time
  data$start_time <- Sys.time()
}

postroute_hook <- function(data, req, res) {
  time_diff <- round(abs(as.numeric(difftime(Sys.time(), data$start_time, units = "secs"))), 4)
  "!DEBUG `write_log_message(req, res, paste('Request executed in', time_diff, 'seconds'))`"
}


check_rstudio_connect <- function() {
  # Return TRUE if running in a traditional RStudio Connect environment
  env_vars <- Sys.getenv()
  Sys.getenv("RSTUDIO_PRODUCT") == "CONNECT" |  # This is only a valid check on recent RSC versions
    "RSTUDIO_CONNECT_HASTE" %in% names(env_vars) |
    getwd() == "/opt/rstudio-connect/mnt/app" |
    Sys.getenv("LOGNAME") == "rstudio-connect" |
    Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect" |
    Sys.getenv("TMPDIR") == "/opt/rstudio-connect/mnt/tmp" |
    grepl("^/opt/rstudio-connect/mnt/tmp", Sys.getenv("R_SESSION_TMPDIR"))
}
