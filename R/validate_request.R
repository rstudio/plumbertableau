# Called on a specific request before a user-defined function is called. This
# takes place in the function generated in tableau_handler().
validate_request <- function(req, args, return) {
  # Not for any particular reason
  force(req)
  force(args)
  force(return)

  optionals <- vapply(args, function(x) x[["optional"]], logical(1))
  opt_idx <- which(optionals)
  req_idx <- which(!optionals)
  min_possible_args <- length(req_idx)
  max_possible_args <- length(args)

  val <- args[seq_len(min(max_possible_args, max(min_possible_args, length(req$body$data))))]

  dat <- req$body$data

  # Check that the same number of values is provided as what is expected
  if (length(val) != length(dat)) {
    err <- paste0(req$PATH_INFO,
                  " expected ",
                  if (min_possible_args == max_possible_args) {
                    min_possible_args
                  } else {
                    paste0("between ", min_possible_args, " and ", max_possible_args)
                  },
                  " arguments but instead received ",
                  length(dat))
    stop(err, call. = FALSE)
  }

  # Check to make sure data types match
  dat_types <- lapply(dat, class)
  expected_types <- vapply(val, function(arg_spec) {
    if (inherits(arg_spec, "tableau_arg_spec")) {
      arg_spec[["type"]]
    } else if (is.character(arg_spec)) {
      arg_spec
    } else {
      stop("Unexpected arg_spec type: ", class(arg_spec)[[1]])
    }
  }, character(1))

  mismatch <- !check_types(dat_types, expected_types)
  if (any(mismatch)) {
    err <- paste0("Mismatched data types found in ",
                  req$PATH_INFO,
                  ": ",
                  paste0("\n - Argument ",
                         which(mismatch),
                         " (",
                         names(val)[mismatch],
                         ") is type '",
                         unlist(dat_types[mismatch]),
                         "' but type '",
                         expected_types[mismatch],
                         "' was expected",
                         collapse = "")
    )
    stop(err, call. = FALSE)
  }

  # Assign dat with names provided
  names(dat) <- names(val)

  # Add missing optionals, with NULL values
  missing_arg_names <- utils::tail(names(args), -length(val))
  missing_args <- stats::setNames(
    rep_len(list(NULL), length(missing_arg_names)),
    missing_arg_names)
  dat <- c(dat, missing_args)

  # Return the renamed data as a list
  dat
}

check_types <- function(actual_types, expected_types) {
  ifelse(expected_types == "any",
    rep_len(TRUE, length(expected_types)),
    actual_types == expected_types
  )
}
