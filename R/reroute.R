reroute <- function(req, res) {
  "!DEBUG `write_log_message(req, res)"
  if (req$PATH_INFO == "/info") {
    "!DEBUG `write_log_message(req, res, 'Responding to /info request')"
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
      req$argsQuery <- parseQS(new_query_string)
      req$args <- c(req$args, req$argsQuery)
      "!DEBUG `write_log_message(req, res, paste('Rerouting /evaluate request to', new_path_info))`"
    }
  }
  plumber::forward()
}
