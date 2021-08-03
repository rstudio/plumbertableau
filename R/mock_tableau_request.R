#' Create a mock JSON request that mimics the request structure of Tableau
#'
#' `mock_tableau_request()` creates a JSON object formatted like a request from
#' Tableau. The JSON object it returns can be pasted directly into the "Try it
#' out" field in the Swagger documentation for an endpoint to test its
#' functionality.
#'
#' Behind the scenes, Tableau sends all requests to the `/evaluate` endpoint.
#' Each request is a JSON object containing two items: `script` and `data`.
#' plumbertableau uses `script` to specify an individual endpoint to call, and
#' passes the arguments in `data` on to the function at that endpoint.
#'
#' @param script String indicating the path to the endpoint to be called
#' @param data A list or dataframe that is serialized to JSON
#' @param ... Additional arguments passed to \code{jsonlite::toJSON()}
#'
#' @return A JSON object that can be passed to a Tableau endpoint
#'
#' @examples
#' mock_tableau_request("/loess/predict", mtcars[,c("hp", "mpg")])
#'
#' @export
mock_tableau_request <- function(script, data, ...) {
  # Verify data is a list
  if (!is.list(data)) stop("data must be a list or data.frame object.", call. = FALSE)
  data <- as.list(data)

  # Verify every entry of data is of the same length - Tableau will only submit
  # data where every array is of the same length
  list_lengths <- unlist(lapply(data, length))
  if(!all(list_lengths == list_lengths[1])) stop("All entries in data must be of equal length.", call. = FALSE)

  names(data) <- paste0("_arg", 1:length(data))
  jsonlite::prettify(
    jsonlite::toJSON(list(
      script = jsonlite::unbox(script),
      data = data
    ),
    ...)
  )
}
