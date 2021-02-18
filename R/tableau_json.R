#' Create a mock JSON request that mimics the behavior of Tableau
#'
#' @param script String indicating the endpoint path
#' @param data A list or dataframe that is serialized to JSON
#' @param ... Additional arguments passed to \code{jsonlite::toJSON()}
#'
#' @return A JSON object that can be passed to a Tableau endpoint
#'
#' @export
tableau_json <- function(script, data, ...) {
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
  ...)
}
