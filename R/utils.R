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

# Utilities for capturing endpoint execution time
preroute_hook <- function(data, req, res) {
  # Capture execution start time
  data$start_time <- Sys.time()
}

postroute_hook <- function(data, req, res) {
  time_diff <- round(abs(as.numeric(difftime(Sys.time(), data$start_time, units = "secs"))), 4)
  "!DEBUG `write_log_message(req, res, paste('Request executed in', time_diff, 'seconds'))`"
}

# Pulled from plumber package to avoid using a non-exported function
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

#' @noRd
#' @importFrom stats setNames
combine_keys <- function(obj, type) {

  keys <- names(obj)
  unique_keys <- unique(keys)

  # If a query string as the same amount of unique keys as keys,
  # then return it as it
  # (`"multi"` type objects MUST be processed, regardless if the unique key count is the same)
  if (
    length(unique_keys) == length(keys) &&
    identical(type, "query")
  ) {
    return(obj)
  }

  vals <- unname(obj)

  cleanup_item <- switch(
    type,
    "query" =
      function(x) {
        unname(unlist(x))
      },
    "multi" =
      function(x) {
        if (length(x) == 1) {
          part <- x[[1]]
          filename <- part$filename
          parsed <- part$parsed

          if (!is.null(filename)) {
            # list(
            #   "myfile.json" = list(
            #     a = 1, b = 2
            #   )
            # )
            return(
              setNames(
                list(parsed),
                filename
              )
            )
          }
          # list(
          #   a = 1, b = 2
          # )
          return(parsed)
        }

        # length is > 1

        has_a_filename <- FALSE
        filenames <- lapply(x, function(part) {
          filename <- part$filename
          if (is.null(filename)) return("")
          has_a_filename <<- TRUE
          filename
        })

        parsed_items <- lapply(unname(x), `[[`, "parsed")

        if (!has_a_filename) {
          # return as is
          return(parsed_items)
        }

        return(setNames(parsed_items, filenames))
      },
    stop("unknown type: ", type)
  )

  # equivalent code output, `split` is much faster with larger objects
  # Testing on personal machine had a breakpoint around 150 letters as query parameters
  ## n <- 150
  ## k <- sample(letters, n, replace = TRUE)
  ## v <- as.list(sample(1L, n, replace = TRUE))
  ## microbenchmark::microbenchmark(
  ##   split = {
  ##     lapply(split(v, k), function(x) unname(unlist(x)))
  ##   },
  ##   not_split = {
  ##     lapply(unique(k), function(x) {
  ##       unname(unlist(v[k == x]))
  ##     })
  ##   }
  ## )
  vals <-
    if (length(unique_keys) > 150) {
      lapply(split(vals, keys), function(items) {
        cleanup_item(items)
      })
    } else {
      # n < 150
      lapply(unique_keys, function(key) {
        cleanup_item(vals[keys == key])
      })
    }
  names(vals) <- unique_keys

  vals
}

# Generate an informational message based on the execution context of the extension
warning_message <- function() {
  message_contents <- NULL
  # If running on RSC, perform checks; if not return NULL
  if (check_rstudio_connect()) {
    # RStudio Connect Details
    # Provide messaging if RSC:
    #  * Is a version that doesn't support Tableau Extensions
    #  * Isn't configured to support Tableau Extensions
    #  * Doesn't have Server.Address configured
    # TODO: Replace this with proper logic once available
    minimum_version <- "1.9.0"

    # Server.Address
    connect_server <- Sys.getenv("CONNECT_SERVER")

    # Does this installation support Tableau Extensions
    connect_support <- Sys.getenv("RSC_TABLEAU")

    # RStudio Connect version
    connect_version <- Sys.getenv("RSC_VERSION")

    if (utils::compareVersion(connect_version, minimum_version) < 0) {
      message_contents <- paste(message_contents,
                                paste0("> **WARNING**: This version of RStudio Connect (",
                                       connect_version,
                                       ") does not support Tableau Analytics Extension APIs. Please upgrade RStudio Connect to version ",
                                       minimum_version,
                                       " or newer.\n"),
                                sep = "\n")
    }
    if (!rlang::is_true(as.logical(connect_support))) {
      message_contents <- paste(message_contents,
                                "> **WARNING**: Tableau Analytics Extension API support is currently disabled in RStudio Connect's configuration.\n",
                                sep = "\n")
    }

    if (connect_server == "") {
      message_contents <- paste(message_contents,
                                "> **WARNING**: The `Server.Address` property is not set in RStudio Connect's configuration.\n",
                                sep = "\n")
    }


    # Send warning message to the console if any of the above are TRUE
    if (!rlang::is_null(message_contents)) {
      message_contents <- paste(
        message_contents,
        "\n\n#### Please reach out to your RStudio Connect administrator to fix these issues.",
        sep = "\n"
      )

      rlang::warn(stringi::stri_replace_all(message_contents, regex = "\\* ?|#+ ", replacement = ""), .frequency = "once", .frequency_id = "rsc_warning")
    }
  }

  message_contents
}


info_message <- function() {
  message_contents <- NULL
  if (stringi::stri_detect(Sys.getenv("DEBUGME"), fixed = "plumbertableau")) {
    message_contents <- paste(
      message_contents,
      "Verbose logging is on. To disable it please remove the `DEBUGME` environment variable or set it to a value that does not include 'plumbertableau'.",
      sep = "\n"
    )
  } else {
    message_contents <- paste(
      message_contents,
      "Verbose logging is off. To enable it please set the environment variable `DEBUGME` to include 'plumbertableau'.",
      sep = "\n"
    )
  }

  message_contents
}

check_rstudio_connect <- function() {
  # Return TRUE if running in a traditional RStudio Connect environment
  Sys.getenv("RSTUDIO_PRODUCT") == "CONNECT"
}
