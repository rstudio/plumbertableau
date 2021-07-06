# A plumber filter for dealing with various details related to RStudio Connect
rsc_filter <- function(req, res) {
  # Parse the endpoint path from header on RStudio Connect
  # TODO: Improve logic for identifying content_path
  # Look into RSTUDIO-CONNECT-APP-BASE-URL
  if (!rlang::is_null(req$HTTP_X_RSC_REQUEST)) {
    full_path <- req$HTTP_X_RSC_REQUEST
    rsc_root <- Sys.getenv("CONNECT_SERVER")
    content_path <- gsub(rsc_root,
                         "",
                         full_path,
                         fixed = TRUE
    )

    # If the path requested is not root (/), strip it from the content path
    if (req$PATH_INFO != "/") {
      content_path <- gsub(req$PATH_INFO,
                           "",
                           content_path,
                           fixed = TRUE
      )
    }
    req$content_path <- content_path
  }

  # Request ID - RStudio Connect sends X-RS-CORRELATION-ID, but we will use the
  # generic X-CORRELATION-ID
  if (!rlang::is_null(req$HTTP_X_RS_CORRELATION_ID)) {
    req$HTTP_X_CORRELATION_ID <- req$HTTP_X_RS_CORRELATION_ID
  }

  plumber::forward()
}
