#' Make an existing Plumber API compliant as a Tableau Analytics Extension
#'
#' @param pr An existing Plumber router
#'
#' @return A modified version of \code{pr} that is compliant with Tableau
#'
#' @examples
#' \dontrun{
#' library(plumber)
#' library(rtab)
#'
#' #* Capitalize incoming text
#' #* @parser json
#' #* @serializer json
#' #* @post /capitalize
#' function(req, res) {
#'   dat <- req$body$data
#'   toupper(dat)
#' }
#'
#' #* @plumber
#' tableau_extension
#' }
#'
#' @export
tableau_extension <- function(pr) {
  # If this is running on RStudio Connect, the original Plumber router should be
  # returned
  if (check_connect()) {
    pr
  } else {
    pr %>%
      plumber::pr_filter("reroute", reroute) %>%
      plumber::pr_get("/info", info)
  }
}
