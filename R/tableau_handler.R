#' Create a Tableau-compliant handler for a function
#'
#' Creates an object that can translate arguments from Tableau to R, and return
#' values from R to Tableau.
#'
#' @param args A named list describing the arguments that are expected from
#'   valid Tableau requests. The names in the named list can be any unique
#'   variable names. The values in the named list must each be either a string
#'   indicating the expected data type for that argument (`"character"`,
#'   `"logical"`, `"numeric"`, or `"integer"`); or better yet, a specification
#'   object created by [arg_spec()]. If an argument should be considered
#'   optional, then its data type should be followed by `?`, like `"numeric?"`.
#' @param return A string indicating the data type that will be returned from
#'   `func` (`"character"`, `"logical"`, `"numeric"`, or `"integer"`); or, a
#'   specification object created by [return_spec()].
#' @param func A function to be used as the handler function. Code in the body
#'   of the function will automatically be able to access Tableau request args
#'   simply by referring to their names in `args`; see the example below.
#'
#' @return A `tableau_handler` object that is a validated version of the
#' provided `func` with additional attributes describing the expected arguments
#' and return values
#'
#' @export
tableau_handler <- function(args, return, func) {
  args <- lapply(args, normalize_argspec)
  validate_argspecs(args)
  return <- normalize_returnspec(return)

  fargs <- getRelevantArgs(args, func)
  unused_args <- setdiff(names(args), names(fargs))
  if (length(unused_args) > 0) {
    warning(call. = FALSE, immediate. = TRUE,
      "The following Tableau arg(s) were declared, but not included as ",
      "function parameters: ",
      paste(collapse = ", ", paste0("'", unused_args, "'"))
    )
  }

  result <- function(req, res, ...) {
    vars <- validate_request(req, args = args, return = return)
    fargs <- rlang::list2(req = req, res = res, !!!vars, ...)
    fargs <- getRelevantArgs(fargs, func)
    do.call(func, fargs)
  }

  attr(result, "tableau_arg_specs") <- args
  attr(result, "tableau_return_spec") <- return
  class(result) <- c("tableau_handler", class(result))
  result
}

#' Describe expected args and return values
#'
#' `arg_spec()` and `return_spec()` are used to create arguments for
#' [tableau_handler()]. They describe the data type of the arg or return value,
#' and can return a human-readable description that can be used to generate
#' documentation.
#'
#' @param type A string indicating the data type that is required for this
#'   argument.
#' @param desc A human-readable description of the argument. Used to generate
#'   documentation.
#' @param optional If `TRUE`, then this argument need not be present in a
#'   request. Defaults to `TRUE` if `type` ends with a `"?"` character.
#'
#' @return A `tableau_arg_spec` object, which is a list containing details about
#' the Tableau argument expectations
#'
#'
#' @export
arg_spec <- function(type = c("character", "integer", "logical", "numeric"),
  desc = "", optional = grepl("\\?$", type)) {

  # We're about to modify `type`, so eval `optional` first
  force(optional)

  type <- sub("\\?$", "", type)

  structure(list(
    type = normalize_type_to_r(type),
    desc = desc,
    optional = optional
  ), class = c("tableau_arg_spec", "list"))
}

param_spec <- function(type = c("character", "integer", "logical", "numeric"),
  desc = "", optional = grepl("\\?$", type), default = NULL) {

  # We're about to modify `type`, so eval `optional` first
  force(optional)

  type <- sub("\\?$", "", type)

  structure(list(
    type = normalize_type_to_r(type),
    desc = desc,
    optional = optional,
    default = default
  ), class = c("tableau_param_spec", "list"))
}

#' @rdname arg_spec
#'
#' @return A `tableau_return_spec` object, which is a list containing details
#' about the values expected to be returned to Tableau
#'
#' @export
return_spec <- function(type = c("character", "integer", "logical", "numeric"),
  desc = "") {

  type <- match.arg(type)

  structure(list(
    type = normalize_type_to_r(type),
    desc = desc
  ), class = c("tableau_return_spec", "list"))
}

# Given a route that may have @tab.* comments, create a tableau_handler object.
infer_tableau_handler <- function(route) {
  func <- route$getFunc()
  if (inherits(func, "tableau_handler")) {
    # Already a handler
    return(func)
  }

  srcref <- attr(func, "srcref", exact = TRUE)
  if (is.null(srcref)) {
    stop(
      call. = FALSE,
      "plumbertableau encountered a plumber endpoint with no srcref; try ",
      "making sure all endpoint functions are defined directly in the plumber ",
      "file"
    )
  }
  comment_lines_df <- get_comments_from_srcref(srcref)
  parsed_comments <- parse_comment_tags(comment_lines_df)


  # Check to see if Tableau args and return values have been provided
  err <- "Tableau argument and return data types must be specified. Please use either #* tableauArg and #* tableauReturn annotations or tableau_handler() to specify Tableau argument and return types."

  if (rlang::is_empty(parsed_comments)) {
    stop(err, call. = FALSE)
  } else if (!("tableauArg" %in% parsed_comments$tag) | !("tableauReturn" %in% parsed_comments$tag)) {
    stop(err, call. = FALSE)
  }

  args <- parsed_comments[parsed_comments$tag == c("tableauArg"), c("line", "remainder")]
  returns <- parsed_comments[parsed_comments$tag == c("tableauReturn"), c("line", "remainder")]

  args <- parse_args_comment_df(args)
  return <- parse_return_comment_df(returns)

  tableau_handler(
    args = args,
    return = return,
    func = func
  )
}

get_comments_from_srcref <- function(srcref) {
  func_start_line <- srcref[[7]]
  srcfile <- attr(srcref, "srcfile", exact = TRUE)
  lineNum <- func_start_line - 1
  file <- getSrcLines(srcfile, 1, lineNum)

  while (lineNum > 0 && (grepl("^#['\\*]", file[lineNum]) || grepl("^\\s*$", file[lineNum]))) {
    lineNum <- lineNum - 1
  }
  line <- seq(from = lineNum + 1, length.out = func_start_line - (lineNum + 1))
  data.frame(line, text = file[line], stringsAsFactors = FALSE)
}

parse_comment_tags <- function(lines_df) {
  lines <- lines_df$text
  m <- regexec("^#['\\*]\\s+@([^\\s]+)\\s*(.*)", lines, perl = TRUE)
  matches <- regmatches(lines, m)

  tag <- sapply(matches, `[`, i = 2)
  remainder <- sapply(matches, `[`, i = 3)
  df <- data.frame(line = lines_df$line, tag, remainder, stringsAsFactors = FALSE)
  df[!is.na(df$tag),]
}

# @param comment_df Data frame with `line` and `remainder` columns
parse_args_comment_df <- function(comment_df) {
  m <- regexec("^\\s*([a-zA-Z0-9._-]+):([^\\s?]+)(\\?)?\\s+(.*)$", comment_df$remainder, perl = TRUE)
  matches <- regmatches(comment_df$remainder, m)

  name <- sapply(matches, `[`, i = 2)
  type <- sapply(matches, `[`, i = 3)
  type <- sub("^\\[(.+)]$", "\\1", type)
  opt <- sapply(matches, `[`, i = 4)
  desc <- sapply(matches, `[`, i = 5)

  bad_lines <- comment_df$line[which(is.na(name))]
  if (length(bad_lines) > 0) {
    stop("Invalid @tableauArgu on line(s) ", paste(bad_lines, collapse = ", "))
  }

  arg_specs <- mapply(name, type, opt, desc, FUN = function(name, type, opt, desc) {
    arg_spec(type = type, desc = desc, optional = ifelse(is.na(opt), FALSE, opt == "?"))
  }, SIMPLIFY = FALSE, USE.NAMES = TRUE)

  # Make sure @tableauArg names are unique
  duplicate_names <- unique(name[which(duplicated(name))])
  if (length(duplicate_names) > 0) {
    stop(call. = FALSE,
      "Duplicate @tableauArg name(s) detected: ",
      paste0(paste0("'", duplicate_names, "'"), collapse = ", "),
      ". Names must be unique."
    )
  }

  arg_specs
}

parse_return_comment_df <- function(comment_df) {
  m <- regexec("^\\s*([^\\s?]+)\\s+(.*)$", comment_df$remainder, perl = TRUE)
  matches <- regmatches(comment_df$remainder, m)

  type <- sapply(matches, `[`, i = 2)
  type <- sub("^\\[(.+)]$", "\\1", type)
  desc <- sapply(matches, `[`, i = 3)

  return_spec(type = type, desc = desc)
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
    "any" = "any",
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
    "any" = "any",
    stop("Unknown type ", type)
  )

  if (!abbrev) {
    c(str = "string", bool = "boolean", real = "real", int = "integer", any = "any")[short]
  } else {
    short
  }
}

normalize_argspec <- function(arg) {
  if (is.character(arg)) {
    arg_spec(arg)
  } else {
    if (!inherits(arg, "tableau_arg_spec")) {
      stop("Invalid argument specification; arg_spec() object or character expected")
    }
    arg
  }
}

normalize_returnspec <- function(return_obj) {
  if (is.character(return_obj)) {
    return_spec(return_obj)
  } else {
    if (!inherits(return_obj, "tableau_return_spec")) {
      stop("Invalid return value specification; return_spec() object or character expected")
    }
    return_obj
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
