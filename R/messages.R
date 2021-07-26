# Gathers environment data about the extension's execution environment and
# returns a warning message about factors that would prevent the extension
# from running correctly, or receiving and responding to requests from
# Tableau.
warning_message <- function() {
  message_contents <- NULL
  continue_checking <- TRUE
  # If running on RSC, perform checks; if not return NULL
  if (check_rstudio_connect()) {
    # RStudio Connect Details
    # Provide messaging if RSC:
    #  * Is a version that doesn't support Tableau Extensions
    #  * Isn't configured to support Tableau Extensions
    #  * Doesn't have Server.Address configured

    # Connect to RStudio Connect API and read server settings
    rsc_client <- connect(allow_downgrade=TRUE)
    return (message_contents)

    if (rsc_client$error_encountered) {
      message_contents <- paste(message_contents,
        "> **ERROR**: Unable to connect to RStudio Connect!", sep = "\n")
      for (i in rsc_client$failure_messages) {
        message_contents <- paste0(message_contents, "> {i}", sep = "\n")
      }
      continue_checking <- FALSE
    }

    if (continue_checking) {
      if (!rsc_client$validate() || rsc_client$error_encountered) {
        message_contents <- paste0(message_contents,
          "> **ERROR**: Unable to connect to RStudio Connect!", sep = "\n")
        for (i in rsc_client$failure_messages) {
          message_contents <- paste0(message_contents, "> {i}", sep = "\n")
        }
        continue_checking <- FALSE
      }
    }

    if (continue_checking) {
      settings <- rsc_client$server_settings()
      if (is.null(settings) || rsc_client$error_encountered) {
        message_contents <- paste0(message_contents,
          "> **ERROR**: Unable to retrieve server settings from RStudio Connect!", sep = "\n")
        for (i in rsc_client$failure_messages) {
          message_contents <- paste0(message_contents, "> {i}", sep = "\n")
        }
        continue_checking <- FALSE
      }
    }

    if (!continue_checking) {
      return(rlang::warn(stringi::stri_replace_all(message_contents, regex = "\\* ?|#+ ", replacement = ""), .frequency = "once", .frequency_id = "rsc_warning"))
    }

    # Server.Address
    connect_server <- Sys.getenv("CONNECT_SERVER")

    if (connect_server == "") {
      message_contents <- paste0(message_contents,
                                "> **WARNING**: The `Server.Address` property is not set in RStudio Connect's configuration.\n",
                                sep = "\n")
    }

    # Does this installation support Tableau Extensions
    connect_support <- settings$tableau_integration_enabled

    # Find connect version
    connect_version <- settings$version

    if (is.null(connect_support)) {
      message_contents <- paste0(message_contents,
                                paste0("> **WARNING**: This version of RStudio Connect (",
                                      connect_version,
                                      ") does not support Tableau Analytics Extension APIs. Please upgrade RStudio Connect."
                                ),
                                sep = "\n")
    } else if (!connect_support) {
      message_contents <- paste0(message_contents,
                                "> **WARNING**: Tableau Analytics Extension API support is currently disabled in RStudio Connect's configuration.",
                                sep = "\n")
    }

    # Send warning message to the console if any of the above are TRUE
    if (!rlang::is_null(message_contents)) {
      message_contents <- paste0(
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
    message_contents <- paste0(
      message_contents,
      "Verbose logging is on. To disable it please remove the `DEBUGME` environment variable or set it to a value that does not include 'plumbertableau'.",
      sep = "\n"
    )
  } else {
    message_contents <- paste0(
      message_contents,
      "Verbose logging is off. To enable it please set the environment variable `DEBUGME` to include 'plumbertableau'.",
      sep = "\n"
    )
  }

  # Provide information about RStudio Connect logging
  # if (check_rstudio_connect()) {
  #   rsc_client <- connect(allow_downgrade=TRUE)
  #   settings <- rsc_client$server_settings()
  #   if (!rlang::is_null(settings$tableau_integration_logging)) {
  #     if (settings$tableau_integration_logging) {
  #       message_contents <- paste0(
  #         message_contents,
  #         "RStudio Connect Tableau Logging is enabled. To disable it please set Tableau.Logging to false in the RStudio Connect configuration file.",
  #         sep = "\n"
  #       )
  #     } else {
  #       message_contents <- paste0(
  #         message_contents,
  #         "RStudio Connect Tableau Logging is disabled To enable it please set Tableau.Logging to true in the RStudio Connect configuration file.",
  #         sep = "\n"
  #       )
  #     }
  #   }
  # }

  message_contents
}
