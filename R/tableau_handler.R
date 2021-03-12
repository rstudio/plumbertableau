#' Create a Tableau compliant handler
#'
#' @param args A named list describing the arguments that are expected from
#'   valid Tableau requests. The names in the named list can be any unique
#'   variable names. The values in the named list must each be either a string
#'   indicating the expected data type for that argument (`"character"`,
#'   `"logical"`, `"numeric"`, or `"integer"`); or better yet, a specification
#'   object created by [arg_spec()]. If an argument should be considered
#'   optional, then its data type should be followed by `?`, like
#'   `"numeric?"`.
#' @param return A string indicating the data type that will be returned from
#'   `func` (`"character"`, `"logical"`, `"numeric"`, or `"integer"`); or, a
#'   specification object created by [arg_spec()].
#' @param func A function to be used as the handler function. Code in the body
#'   of the function will automatically be able to access Tableau request args
#'   simply by referring to their names in `args`; see the example below.
#' @export
tableau_handler <- function(args, return, func) {
  result <- function(req, res, ...) {
    vars <- validate_request(req, !!!args)

    # Copy the func, leave the original unchanged
    func_local <- func
    environment(func_local) <- list2env(vars, parent = environment(func))
    # TODO: .env and .data might want to be real rlang pronouns?
    environment(func_local)$.env <- environment(func)
    environment(func_local)$.data <- vars

    fargs <- list(req = req, res = res, ...)
    fargs <- getRelevantArgs(fargs, func_local)
    do.call(func_local, fargs)
  }
  argspecs <- normalize_argspecs(args)
  validate_argspecs(argspecs)
  returnspecs <- normalize_argspecs(list("return" = return))
  attr(result, "tableau_arg_specs") <- argspecs
  attr(result, "tableau_return_spec") <- returnspecs[[1]]
  result
}

#' Create an argument specification object
#'
#' @param type A string indicating the data type that is required for this
#'   argument.
#' @param desc A human-readable description of the argument. Used to generate
#'   documentation.
#' @param optional If `TRUE`, then this argument need not be present in a
#'   request.
#'
#' @export
arg_spec <- function(type = c("character", "integer", "logical", "numeric"),
  desc = "", optional = grepl("\\?$", type)) {

  # We're about to modify `type`, so eval `optional` first
  force(optional)

  type <- sub("\\?$", "", type)

  structure(list(
    type = type,
    desc = desc,
    optional = optional
  ), class = "tableau_arg_spec")
}

# Copied from Plumber source code
getRelevantArgs <- function(args, func) {
  # Extract the names of the arguments this function supports.
  fargs <- names(formals(func))

  if (length(fargs) == 0) {
    # no matches
    return(list())
  }

  # fast return
  # also works with unnamed arguments
  if (identical(fargs, "...")) {
    return(args)
  }

  # If only req and res are found in function definition...
  # Only call using the first matches of req and res.
  if (all(fargs %in% c("req", "res"))) {
    ret <- list()
    # using `$` will retrieve the 1st occurance of req,res
    # args$req <- req is used within `Plumber$route()`
    if ("req" %in% fargs) {
      ret$req <- args$req
    }
    if ("res" %in% fargs) {
      ret$res <- args$res
    }
    return(ret)
  }

  # The remaining code MUST work with unnamed arguments
  # If there is no `...`, then the unnamed args will not be in `fargs` and will be removed
  # If there is `...`, then the unnamed args will not be in `fargs` and will be passed through

  if (!("..." %in% fargs)) {
    # Use the named arguments that match, drop the rest.
    args <- args[names(args) %in% fargs]
  }

  # dedupe matched formals
  arg_names <- names(args)
  is_farg <- arg_names %in% fargs
  # keep only the first matched formal argument (and all other non-`farg` params)
  args <- args[(is_farg & !duplicated(arg_names)) | (!is_farg)]

  args
}

normalize_type_to_r <- function(type = c("character", "string", "str",
  "logical", "boolean", "bool",
  "numeric", "real",
  "integer", "int")) {
  switch(type,
    "character" =, "string" =, "str" = "character",
    "logical" =, "boolean" =, "bool" = "logical",
    "numeric" =, "real" = "numeric",
    "integer" =, "int" = "integer",
    stop("Unknown type ", type)
  )
}

normalize_type_to_tableau <- function(type = c("character", "string", "str",
  "logical", "boolean", "bool",
  "numeric", "real",
  "integer", "int"), abbrev = FALSE) {

  short <- switch(type,
    "character" =, "string" =, "str" = "str",
    "logical" =, "boolean" =, "bool" = "bool",
    "numeric" =, "real" = "real",
    "integer" =, "int" = "int",
    stop("Unknown type ", type)
  )

  if (!abbrev) {
    c(str = "string", bool = "boolean", real = "real", int = "integer")[short]
  } else {
    short
  }
}

normalize_argspecs <- function(args) {
  setNames(
    mapply(names(args), args, FUN = normalize_argspec, SIMPLIFY = FALSE, USE.NAMES = FALSE),
    names(args)
  )
}

normalize_argspec <- function(name, arg) {
  if (is.character(arg)) {
    arg_spec(arg)
  } else {
    # TODO: Validate class/shape
    arg
  }
}

validate_argspecs <- function(args) {
  if (length(args) == 0) {
    return(invisible())
  }

  # Enforce all optional parameters coming after required parameters
  optionals <- vapply(args, function(x) x[["optional"]], logical(1))
  opt_idx <- which(optionals)
  req_idx <- which(!optionals)
  if (length(opt_idx) > 0 && length(req_idx) > 0) {
    bad_req_idx <- req_idx[req_idx > min(opt_idx)]
    if (length(bad_req_idx) > 0) {
      stop("Required arg '", names(args)[[bad_req_idx[1]]], "' comes ",
        "after optional arg '", names(args)[[min(opt_idx)]], "'. ",
        "Required args must always come before any optional args.")
    }
  }
}
