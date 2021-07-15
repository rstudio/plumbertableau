# Gathers environment data about the extension's execution environment and
# returns a warning message about factors that would prevent the extension
# from running correctly, or receiving and responding to requests from
# Tableau.
warning_message <- function() {
  message_contents <- NULL
  # If running on RSC, perform checks; if not return NULL
  if (check_rstudio_connect()) {
    # RStudio Connect Details
    # Provide messaging if RSC:
    #  * Is a version that doesn't support Tableau Extensions
    #  * Isn't configured to support Tableau Extensions
    #  * Doesn't have Server.Address configured
    # TODO: Replace this with proper logic once available
    minimum_version <- "1.9.0"

    # Server.Address
    connect_server <- Sys.getenv("CONNECT_SERVER")

    # Does this installation support Tableau Extensions
    connect_support <- Sys.getenv("RSC_TABLEAU")

    # RStudio Connect version
    connect_version <- Sys.getenv("RSC_VERSION")

    if (utils::compareVersion(connect_version, minimum_version) < 0) {
      message_contents <- paste(message_contents,
                                paste0("> **WARNING**: This version of RStudio Connect (",
                                       connect_version,
                                       ") does not support Tableau Analytics Extension APIs. Please upgrade RStudio Connect to version ",
                                       minimum_version,
                                       " or newer.\n"),
                                sep = "\n")
    }
    if (!rlang::is_true(as.logical(connect_support))) {
      message_contents <- paste(message_contents,
                                "> **WARNING**: Tableau Analytics Extension API support is currently disabled in RStudio Connect's configuration.\n",
                                sep = "\n")
    }

    if (connect_server == "") {
      message_contents <- paste(message_contents,
                                "> **WARNING**: The `Server.Address` property is not set in RStudio Connect's configuration.\n",
                                sep = "\n")
    }


    # Send warning message to the console if any of the above are TRUE
    if (!rlang::is_null(message_contents)) {
      message_contents <- paste(
        message_contents,
        "\n\n#### Please reach out to your RStudio Connect administrator to fix these issues.",
        sep = "\n"
      )

      rlang::warn(stringi::stri_replace_all(message_contents, regex = "\\* ?|#+ ", replacement = ""), .frequency = "once", .frequency_id = "rsc_warning")
    }
  }

  message_contents
}

# Gathers information about the extensions's execution environment and returns
# a message with information on factors that modify the extensions's
# behavior
info_message <- function() {
  message_contents <- NULL
  if (stringi::stri_detect(Sys.getenv("DEBUGME"), fixed = "plumbertableau")) {
    message_contents <- paste(
      message_contents,
      "Verbose logging is on. To disable it please remove the `DEBUGME` environment variable or set it to a value that does not include 'plumbertableau'.",
      sep = "\n"
    )
  } else {
    message_contents <- paste(
      message_contents,
      "Verbose logging is off. To enable it please set the environment variable `DEBUGME` to include 'plumbertableau'.",
      sep = "\n"
    )
  }

  message_contents
}
