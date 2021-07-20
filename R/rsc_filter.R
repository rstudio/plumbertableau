# A plumber filter for dealing with various details related to RStudio Connect
rsc_filter <- function(req, res) {
  # Parse the endpoint path from header on RStudio Connect
  req$content_path <- NULL
  if (!rlang::is_null(req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL)) {
    base_url <- req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL
    rsc_root <- Sys.getenv("CONNECT_SERVER")
    content_path <- gsub(rsc_root,
                         "",
                         base_url,
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

    # Ensure path starts with '/'
    if (!stringi::stri_startswith(content_path, "/")) content_path <- paste0("/", content_path)
    req$content_path <- content_path
  }

  # Request ID - RStudio Connect sends X-RS-CORRELATION-ID, but we will use the
  # generic X-CORRELATION-ID
  if (!rlang::is_null(req$HTTP_X_RS_CORRELATION_ID)) {
    req$HTTP_X_CORRELATION_ID <- req$HTTP_X_RS_CORRELATION_ID
  }

  plumber::forward()
}
