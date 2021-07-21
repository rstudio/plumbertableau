# A plumber filter for dealing with various details related to RStudio Connect
# TODO: Can the content_path be set at the process level instead of the request level
rsc_filter <- function(req, res) {
  # Request ID - RStudio Connect sends X-RS-CORRELATION-ID, but we will use the
  # generic X-CORRELATION-ID
  if (!rlang::is_null(req$HTTP_X_RS_CORRELATION_ID)) {
    req$HTTP_X_CORRELATION_ID <- req$HTTP_X_RS_CORRELATION_ID
  }

  plumber::forward()
}
