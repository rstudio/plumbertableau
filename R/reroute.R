reroute <- function(req, res) {
  if (req$PATH_INFO == "/evaluate") {
    body <- jsonlite::fromJSON(req$postBody)
    if ("script" %in% names(body)) {
      if (body$script == "return int(1)") {
        return(1L)
      }
      # If the leading / is present, overwrite the path. Otherwise, add the
      # leading /
      if (grepl("^/", body$script)) {
        req$PATH_INFO <- body$script
      } else {
        req$PATH_INFO <- paste0("/", body$script)
      }
    }
  }
  plumber::forward()
}
