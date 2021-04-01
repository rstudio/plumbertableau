#' Perform a series of checks to determine if the current code is executing on
#' RStudio Connect. This intentionally casts a wide net since there's no
#' persistent and reliable way to check of RStudio Connect is executing the R
#' session
check_connect <- function() {
  env_vars <- Sys.getenv()
  "RSTUDIO_CONNECT_HASTE" %in% names(env_vars) |
    getwd() == "/opt/rstudio-connect/mnt/app" |
    Sys.getenv("LOGNAME") == "rstudio-connect" |
    Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect" |
    Sys.getenv("TMPDIR") == "/opt/rstudio-connect/mnt/tmp" |
    grepl("^/opt/rstudio-connect/mnt/tmp", Sys.getenv("R_SESSION_TMPDIR"))
}

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