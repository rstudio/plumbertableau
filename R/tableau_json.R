#' Create a mock JSON request that mimics the request structure of Tableau
#'
#' @param script String indicating the endpoint path
#' @param data A list or dataframe that is serialized to JSON
#' @param pretty Whether or not to 'prettify' the JSON output
#' @param ... Additional arguments passed to \code{jsonlite::toJSON()}
#'
#' @return A JSON object that can be passed to a Tableau endpoint
#'
#' @examples
#' mock_tableau_request("/loess/predict", mtcars[,c("hp", "mpg")])
#'
#' @export
mock_tableau_request <- function(script, data, pretty = TRUE, ...) {
  # Verify data is a list
  if (!is.list(data)) stop("data must be a list or data.frame object.", call. = FALSE)
  data <- as.list(data)

  # Verify every entry of data is of the same length - Tableau will only submit
  # data where every array is of the same length
  list_lengths <- unlist(lapply(data, length))
  if(!all(list_lengths == list_lengths[1])) stop("All entries in data must be of equal length.", call. = FALSE)

  names(data) <- paste0("_arg", 1:length(data))
  jsonlite::toJSON(list(
    script = jsonlite::unbox(script),
    data = data
  ),
  pretty = pretty,
  ...)
}
