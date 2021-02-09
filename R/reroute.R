reroute <- function(req, res) {
  if ((req$PATH_INFO == "/evaluate" | !is.null(req$HTTP_TABPY_CLIENT)) && req$REQUEST_METHOD == "POST") {
    # Parse out postBody
    body <- jsonlite::fromJSON(req$postBody)
    # Pass arguments to user functions and preserve existing arguments
    # We want to do this here b/c req$postBody is already parsed
    # This should only fire for Tableau requests
    req$args <- c(req$args, unname(body$data))
    if ("script" %in% names(body) && !check_connect()) {
      # This satisfies a Tableau requirement
      # See https://tableau.github.io/analytics-extensions-api/docs/ae_known_issues.html
      if (body$script == "return int(1)") {
        return(1L)
      }
      # Create the new path
      new_path <- body$script
      if (!startsWith(new_path, "/")) new_path <- paste0("/", new_path)
      req$PATH_INFO <- new_path
    }
  }
  plumber::forward()
}
