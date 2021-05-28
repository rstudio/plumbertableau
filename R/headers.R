# A plumber filter for deailing with various HTTP headers in the incoming request
headers <- function(req, res) {
  # Parse the endpoint path from header on RStudio Connect
  if (!rlang::is_empty(req$HTTP_X_RSC_REQUEST)) {
    full_path <- req$HTTP_X_RSC_REQUEST
    rsc_root <- Sys.getenv("CONNECT_SERVER")
    vanity_path <- gsub(rsc_root,
                        "",
                        gsub(req$PATH_INFO,
                             "",
                             full_path)
    )
    req$vanity_path <- vanity_path
  }

  plumber::forward()
}
