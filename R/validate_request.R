#' Validate an incoming Tableau API request
#'
#' @param req The \code{req} object of a Plumber request
#' @param ... Named values to parse out of \code{req$body$data} and the expected
#' data type
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @return A list of named values parsed from \code{req$body$data}
#'
#' @export
validate_request <- function(req, ...) {
  val <- rlang::list2(...)
  dat <- req$body$data

  # Check that the same number of values is provided as what is expected
  if (length(val) != length(dat)) {
    err <- paste0(req$PATH_INFO,
                  " expected ",
                  length(val),
                  " arguments but instead received ",
                  length(dat))
    stop(err, call. = FALSE)
  }

  # Check to make sure data types match
  dat_types <- lapply(dat, class)
  mismatch <- unlist(
    Map(function(x, y) x != y,
        val,
        dat_types)
  )
  if (any(mismatch)) {
    err <- paste0("Mismatched data types found in ",
                  req$PATH_INFO,
                  ": ",
                  paste0("\n - Argument ",
                         which(mismatch),
                         " (",
                         names(val)[mismatch],
                         ") is type ",
                         unlist(val[mismatch]),
                         " but type ",
                         unlist(dat_types[mismatch]),
                         " was expected",
                         collapse = "")
    )
    stop(err, call. = FALSE)
  }

  # Assign dat with names provided
  names(dat) <- names(val)

  # Return the renamed data as a list
  dat
}
