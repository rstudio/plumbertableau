#' Programatically invoke a Tableau extension function
#'
#' Simulates invoking a Tableau extension function from a Tableau calculated
#' field `SCRIPT_*` call. Intended for unit testing of plumbertableau extensions.
#'
#' @param pr Either a [tableau_extension] style Plumber router object, or, the
#'   filename of a plumber.R that implements a Tableau extension.
#' @param script The script string that identifies the plumber route to invoke.
#'   (Equivalent to the first argument to `SCRIPT_STR`, et al., in Tableau.) URL
#'   query parameters are allowed.
#' @param ... Zero or more unnamed arguments to be passed to the script.
#' @param .toJSON_args Additional options that should be passed to
#'   [jsonlite::toJSON()] when the `...` arguments are serialized; for example,
#'   `pretty = TRUE` or `digits = 8`.
#' @param .quiet If `TRUE`, do not print response bodies when errors occur.
#'
#' @return The object that was returned from the request, JSON-decoded using
#'   `jsonlite::parse_json`.
#'
#' @examples
#' pr_path <- system.file("plumber/stringutils/plumber.R",
#'   package = "plumbertableau")
#'
#' tableau_invoke(pr_path, "/lowercase", LETTERS[1:5])
#'
#' @export
tableau_invoke <- function(pr, script, ..., .toJSON_args = NULL, .quiet = FALSE) {
  # Prevents Swagger UI from launching--sometimes. (It doesn't work when pr is
  # an actual router object, since you need to have these set at the time of
  # Plumber router initialization.)
  op <- options(plumber.docs.callback = NULL, plumber.swagger.url = NULL)
  on.exit(options(op), add = TRUE)

  if (is.character(pr)) {
    pr <- plumber::plumb(pr)
  }
  payload <- encode_payload(script, ..., .toJSON_args = .toJSON_args)

  port <- httpuv::randomPort()

  url <- paste0("http://localhost:", port, "/evaluate")

  result <- NULL
  is_error <- FALSE
  later_handle <- later::later(~{
    h <- curl::new_handle(
      url = url,
      post = TRUE,
      postfieldsize = length(payload),
      postfields = payload
    )
    curl::handle_setheaders(h, "Content-Type" = "application/json")

    p <- promises::then(curl_async(h), ~{
      resp <- .
      if (!isTRUE(resp$status_code == 200)) {
        if (!isTRUE(.quiet)) {
          message(rawToChar(resp$content))
        }
        stop("Unexpected status code: ", resp$status_code)
      }
      if (!identical(resp$type, "application/json")) {
        stop("Response had unexpected content type: ", resp$type)
      }

      result <<- jsonlite::parse_json(rawToChar(resp$content), simplifyVector = TRUE)
      is_error <<- FALSE
    })
    p <- promises::catch(p, ~{
      result <<- .
      is_error <<- TRUE
    })
    p <- promises::finally(p, ~{
      # Make pr$run() return
      httpuv::interrupt()
    })
  })
  # In case of error
  on.exit(later_handle(), add = TRUE)

  pr$run(port = port, quiet = TRUE, swaggerCallback = NULL)

  if (is_error) {
    stop(result)
  } else {
    result
  }
}

encode_payload <- function(script, ..., .toJSON_args, raw = TRUE) {
  data <- rlang::list2(...)
  if (!is.null(names(data))) {
    stop("tableau_invoke requires ... arguments to be unnamed")
  }
  data <- lapply(data, I)
  if (length(data) > 0) {
    names(data) <- paste0("_arg", seq_len(length(data)))
  }
  payload <- list(
    data = data,
    script = jsonlite::unbox(script)
  )

  json <- do.call(jsonlite::toJSON, c(list(x = payload, na = "null"), .toJSON_args))
  json <- enc2utf8(json)
  if (raw == TRUE) {
    charToRaw(json)
  } else {
    json
  }
}

curl_async <- function(handle, polling_interval = 0.1) {
  p <- promises::promise(function(resolve, reject) {
    curl::multi_add(handle, done = resolve, fail = reject)
  })

  p <- promises::finally(p, ~{
    stopifnot(!is.null(later_handle))
    later_handle()
  })

  later_handle <- NULL
  poll <- function() {
    curl::multi_run(timeout = 0, poll = TRUE)
    later_handle <<- later::later(poll, polling_interval)
  }
  poll()

  stopifnot(!is.null(later_handle))

  p
}
