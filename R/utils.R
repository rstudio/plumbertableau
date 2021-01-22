#' Perform a series of checks to determine if the current code is executing on
#' RStudio Connect. This intentionally casts a wide net since there's no
#' persistent and reliable way to check of RStudio Connect is executing the R
#' session
check_connect <- function() {
  env_vars <- Sys.getenv()
  "RSTUDIO_CONNECT_HASTE" %in% names(env_vars) |
    getwd() == "/opt/rstudio-connect/mnt/app" |
    Sys.getenv("LOGNAME") == "rstudio-connect" |
    Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect" |
    Sys.getenv("TMPDIR") == "/opt/rstudio-connect/mnt/tmp" |
    grepl("^/opt/rstudio-connect/mnt/tmp", Sys.getenv("R_SESSION_TMPDIR"))
}
