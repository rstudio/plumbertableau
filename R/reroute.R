reroute <- function(req, res) {
  if (req$PATH_INFO == "/evaluate") {
    body <- jsonlite::fromJSON(req$postBody)
    if ("script" %in% names(body)) {
      # This satisfies a Tableau requirement
      # See https://tableau.github.io/analytics-extensions-api/docs/ae_known_issues.html
      if (body$script == "return int(1)") {
        return(1L)
      }
      new_path <- body$script
      if (!startsWith(new_path, "/")) new_path <- paste0("/", new_path)
      req$PATH_INFO <- new_path
    }
  }
  plumber::forward()
}
