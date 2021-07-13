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


# Pulled from plumber package to avoid using a non-exported function. It parses the 
parseQS <- function(qs){

  if (is.null(qs) || length(qs) == 0L || qs == "") {
    return(list())
  }

  # Looked into using webutils::parse_query()
  # Currently not pursuing `webutils::parse_query` as it does not handle Encoding issues handled below
  # (Combining keys are also not handled by `webutils::parse_query`)

  qs <- stri_replace_first_regex(qs, "^[?]", "")
  qs <- chartr("+", " ", qs)

  args <- stri_split_fixed(qs, "&", omit_empty = TRUE)[[1L]]
  kv <- lapply(args, function(x) {
    # returns utf8 strings
    httpuv::decodeURIComponent(stri_split_fixed(x, "=", omit_empty = TRUE)[[1]])
  })
  kv <- kv[vapply(kv, length, numeric(1)) == 2] # Ignore incompletes

  if (length(kv) == 0) {
    # return a blank list of args if there is nothing to parse
    return(list())
  }

  keys <- vapply(kv, `[`, character(1), 1)
  kenc <- unique(Encoding(keys))
  if (any(kenc != "unknown")) {
    # https://github.com/rstudio/plumber/pull/314#discussion_r239992879
    non_ascii <- setdiff(kenc, "unknown")
    warning(
      "Query string parameter received in non-ASCII encoding. Received: ",
      paste0(non_ascii, collapse = ", ")
    )
  }

  vals <- lapply(kv, `[`, 2)
  names(vals) <- keys

  # If duplicates, combine
  combine_keys(vals, type = "query")
}
