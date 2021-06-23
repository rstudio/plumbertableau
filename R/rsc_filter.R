# A plumber filter for dealing with various details related to RStudio Connect
rsc_filter <- function(req, res) {
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

  # Request ID - RStudio Connect sends X-RS-CORRELATION-ID, but we will use the
  # generic X-CORRELATION-ID
  if (!rlang::is_null(req$HTTP_X_RS_CORRELATION_ID)) {
    req$HTTP_X_CORRELATION_ID <- req$HTTP_X_RS_CORRELATION_ID
  }

  # Provide messaging if RSC doesn't support Tableau extensions
  connect_support <- Sys.getenv("RSC_TABLEAU")
  if (!rlang::is_true(connect_support) && Sys.getenv("CONNECT_SERVER") != "") {
    rsc_support_message <<- "This installation of RStudio Connect does not currently support Tableau Analytics Extension APIs. Please reach out to your RStudio Connect administrator."
    rlang::inform(rsc_support_message, .frequency = "once", .frequency_id = "rsc_warning")
  }

  plumber::forward()
}
