#' Make an existing Plumber API compliant as a Tableau Analytics Extension
#'
#' @param path The root path to use to mount existing Plumber endpoints to. This
#' allows local development to mirror the deployed API when it is deployed to
#' RStudio Connect.
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
tableau_extension <- function(path = "/") {
  function(pr) {
    # If this is running on RStudio Connect, the original Plumber router should be
    # returned
    if (check_connect()) {
      return(pr)
    }
    # Add Tableau boilerplate
    if (length(path) == 0) stop("Path must be a string beginning with /")
    if (!startsWith(path, "/")) path <- paste0("/", path)
    if (endsWith(path, "/") && nchar(path) > 1) path <- sub("/$", "", path)

    # Check for Tableau compliance
    # Every route accepts POST requests
    if (path != "/") {
      # Change router path to supplied path
      lapply(pr$endpoints, function(routes) {
        # Modify route in place
        lapply(routes, function(route) {
          if (!("POST" %in% route$verbs)) warning("Tableau endpoints must accept POST requests")
          # This works b/c route is an R6 object
          route$path <- paste0(path, route$path)
          # Address internal Plumber regex
          route$.__enclos_env__$private$regex <- plumber:::createPathRegex(route$path, route$getFuncParams())
        })
      })
      # Change existing mounts to be mounted under path
      Map(pr$mounts, names(pr$mounts), f = function(mount, mount_path) {
        pr$unmount(mount_path)
        pr$mount(paste0(path, mount_path), mount)
      })
    }

    pr %>%
      plumber::pr_filter("reroute", reroute) %>%
      plumber::pr_get("/info", info) %>%
      plumber::pr_set_api_spec(tableau_openapi)
  }
}
