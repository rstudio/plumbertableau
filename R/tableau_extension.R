#' Make an existing Plumber API compliant as a Tableau Analytics Extension
#'
#' @param warnings Whether or not to provide warnings about non-Tableau compliant
#' endpoints.
#'
#' @return A function that can be used with \code{#* @plumber}
#'
#' @examples
#' \dontrun{
#' library(plumber)
#' library(plumbertableau)
#'
#' #* Capitalize incoming text
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
tableau_extension <- function(warnings = TRUE) {
  function(pr) {
    if (warnings) {
      lapply(pr$routes, function(route) {
        check_route(route)
      })
    }

    # Infer Tableau handler information
    lapply(pr$endpoints, function(routes) {
      # Modify route in place
      lapply(routes, function(route) {
        route$.__enclos_env__$private$func <- infer_tableau_handler(route)
      })
    })

    pr %>%
      plumber::pr_get("/", create_user_guide(pr), serializer = plumber::serializer_html()) %>%
      plumber::pr_static("/__plumbertableau_assets__",
        system.file("www", package = "plumbertableau", mustWork = TRUE)) %>%
      plumber::pr_filter("rsc_filter", rsc_filter) %>%
      plumber::pr_filter("reroute", reroute) %>%
      plumber::pr_hooks(list(
        preroute = preroute_hook,
        postroute = postroute_hook
      )) %>%
      plumber::pr_set_api_spec(tableau_openapi(pr)) %>%
      plumber::pr_set_error(error_handler) %>%
      plumber::pr_set_parsers("json") %>%
      plumber::pr_set_serializer(plumber::serializer_unboxed_json())
  }
}
