# A plumber filter for deailing with various HTTP headers in the incoming request
headers <- function(req, res) {
  # Parse the endpoint path from header on RStudio Connect
  if (!rlang::is_null(req$HTTP_X_RSC_REQUEST)) {
    full_path <- req$HTTP_X_RSC_REQUEST
    rsc_root <- Sys.getenv("CONNECT_SERVER")
    vanity_path <- gsub(rsc_root,
                        "",
                        full_path,
                        fixed = TRUE
    )

    # If the path requested is not root (/), strip it from the vanity path
    if (req$PATH_INFO != "/") {
      vanity_path <- gsub(req$PATH_INFO,
                          "",
                          vanity_path,
                          fixed = TRUE
      )
    }
    req$vanity_path <- vanity_path
  }

  plumber::forward()
}
