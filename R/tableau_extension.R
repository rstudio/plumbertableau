#' Make an existing Plumber API compliant as a Tableau Analytics Extension
#'
#' @param path The root path to use to mount existing Plumber endpoints to
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
#' tableau_extension()
#' }
#'
#' @export
tableau_extension <- function(path = "/") {
  function(pr) {
    # If this is running on RStudio Connect, the original Plumber router should be
    # returned
    if (check_connect()) {
      pr
    } else {
      # Add Tableau boilerplate
      if (path != "/") {
        # Mount a copy of the router under the supplied path
        pr <- plumber::pr_mount(pr, standardize_path(path), pr$clone())
      }
      pr %>%
        plumber::pr_filter("reroute", reroute) %>%
        plumber::pr_get("/info", info)
    }
  }
}
