#' Create a Tableau compliant handler
#'
#' @param args A named list detailing the expected names and types of incoming
#' data from Tableau
#' @param func A function to be used as the handler function
#' @export
tableau_handler <- function(args, func) {
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
  attr(result, "tableau_arg_spec") <- args
  result
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
