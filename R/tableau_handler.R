#' @export
tableau_handler <- function(args, func) {
  result <- function(req, res) {
    vars <- validate_request(req, !!!args)

    # Copy the func, leave the original unchanged
    func_local <- func
    environment(func_local) <- list2env(vars, parent = environment(func))
    # TODO: .env and .data might want to be real rlang pronouns?
    environment(func_local)$.env <- environment(func)
    environment(func_local)$.data <- vars

    func_local(req, res)
  }
  attr(result, "tableau_arg_spec") <- args
  result
}
