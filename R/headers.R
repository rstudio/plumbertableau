# A plumber filter for deailing with various HTTP headers in the incoming request
headers <- function(req, res) {
  # Request ID - RStudio Connect sends X-RS-CORRELATION-ID, but we will use the
  # generic X-CORRELATION-ID
  if (!rlang::is_null(req$HTTP_X_RS_CORRELATION_ID)) {
    req$HTTP_X_CORRELATION_ID <- req$HTTP_X_RS_CORRELATION_ID
  }
  plumber::forward()
}
