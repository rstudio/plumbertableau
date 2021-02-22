reroute <- function(req, res) {
  if (req$PATH_FINO == "/info") {
    return(info())
  }
  if (req$PATH_INFO == "/evaluate") {
    body <- jsonlite::fromJSON(req$postBody)
    if ("script" %in% names(body)) {
      # This satisfies a Tableau requirement
      # See https://tableau.github.io/analytics-extensions-api/docs/ae_known_issues.html
      if (body$script == "return int(1)") {
        return(1L)
      }
      # Create the new path
      new_path <- body$script
      if (!startsWith(new_path, "/")) new_path <- paste0("/", new_path)

      new_path_info <- sub("\\?.*", "", new_path)
      new_query_string <- sub("^[^?]*", "", new_path)
      req$PATH_INFO <- new_path_info
      req$QUERY_STRING <- new_query_string

      # Yuck. The queryStringFilter will have already run.
      req$argsQuery <- plumber:::parseQS(new_query_string)
      req$args <- c(req$args, req$argsQuery)
    }
  }
  plumber::forward()
}
