# Gathers environment data about the extension's execution environment and
# returns a warning message about factors that would prevent the extension
# from running correctly, or receiving and responding to requests from
# Tableau.
warning_message <- function() {
  message_contents <- NULL

  server <- Sys.getenv("CONNECT_SERVER", NA_character_)
  "!DEBUG Environment Variable CONNECT_SERVER = `server`"
  if (is.na(server)) {
    "!DEBUG Problem: CONNECT_SERVER not defined within environment variables"
    message_contents <- paste0(
      message_contents,
      "Problem: CONNECT_SERVER not defined within environment variables. To resolve this, have your system administrator confirm Applications.DefaultServerEnv is enabled and that Server.Address has been defined within the rstudio-connect.gcfg file on the server.",
      sep = "\n"
    )
  } else if (is.null(httr::parse_url(server)$scheme)) {
    "!DEBUG Problem: Environment Variable CONNECT_SERVER does not specify the protocol (https:// or http://)."
    message_contents <- paste0(
      message_contents,
      paste0("Problem: Environment Variable CONNECT_SERVER (value = ", server, " ) does not specify the protocol (https:// or http://). To resolve this, have your system administrator confirm that Server.Address has been configured with the proper format within the rstudio-connect.gcfg file on the server."),
      sep = "\n"
    )
  }

  api_key = Sys.getenv("CONNECT_API_KEY", NA_character_)
  # Do not output the API KEY value..
  if (is.na(api_key)) {
    "!DEBUG Problem: CONNECT_API_KEY not defined within environment variables"
    message_contents <- paste0(
      message_contents,
      "Problem: CONNECT_API_KEY not defined within environment variables. To resolve this, have your administrator check if Applications.DefaultAPIKeyEnv is disabled ithin the rstudio-connect.gcfg file on the server. If it is, then you will either need to have it enabled or you will need to create an API KEY and add it within an environment variable explicitly within Connect.",
      sep = "\n"
    )
  }
  use_http = Sys.getenv("PLUMBERTABLEAU_USE_HTTP", "FALSE")
  "!DEBUG Environment Variable PLUMBERTABLEAU_USE_HTTP = `use_http`"
  # No checks, will just need it for the request.
  if (use_http == "TRUE") {
    server <- sub("https://", "http://", server)
  }
  "!DEBUG After possible https downgrade, server URL is now `server`"

  # Get Server Settings endpoint
  # Confirm that the server address ends in a /
  if (substr(server,(nchar(server)+1)-1,nchar(server)) != "/") {
    server <- paste0(server, "/")
  }
  url <- paste0(server, "__api__/server_settings")
  server_settings <- NULL
  result <- tryCatch (
    {
       "!DEBUG Sending GET request to `url`"
      response <- httr::GET(
        url,
        httr::add_headers(Authorization = paste0("Key ", api_key)),
        httr::write_memory()
      )
      list(success=TRUE, response=response)
    },
    error = function(err) {
      "!DEBUG GET response threw an exception: `err`"
      # Problem: request failed, with error = err
      return (list(
        success=FALSE, 
        message=paste0(
          message_contents,
          paste0("### Issue Detected:\n#### Problem:\nAPI request to ", server, " failed. Error: ", err, 
            ".\n#### Possible Solutions:\nTo resolve this issue, confirm there is connectivity between the server itself and the address assigned to it: ", server, 
            ". If using HTTPS along with self-signed certificates, you may need to allow the plumbertableau package to use HTTP instead, ", 
            "by defining `PLUMBERTABLEAU_USE_HTTP = TRUE` within your environment variables."),
          sep = "\n"
        )
      ))
      # Resolve: If using self-signed certificates, define PLUMBERTABLEAU_USE_HTTP = TRUE if able..
    }
  )
  if (!result$success) {
    "!DEBUG Detected that an error has been returned from exception: `result`"
    message_contents <- paste0(
      message_contents,
      result$message,
      sep = "\n"
    )
  } else {
    "!DEBUG GET response has returned `httr::http_status(result$response)$category`, `httr::http_status(result$response)$reason`, `httr::http_status(result$response)$message`"
    if (httr::http_error(result$response)) {
      "!DEBUG GET response returned an error"
      # NEED TO enhance the message below to include next steps...
      message_contents <- paste0(
        message_contents,
        paste0("Problem: API request to ", server, " failed. Response: ", httr::http_status(result$response)$reason, ", ", httr::http_status(result$response)$message, "."),
        sep = "\n"
      )
      # Problem: request failed, with error in response
      # Resolve: as appropriate...
    } else {
      server_settings <- httr::content(result$response, as="parsed")
      "!DEBUG GET response was successful. Server settings = `server_settings`"
    }
  }

  "!DEBUG message_contents after GET request: `message_contents`"
  if (!is.null(message_contents)) {
    return (message_contents)
  }

  # Does this installation support Tableau Extensions
  "!DEBUG server_settings$tableau_integration_enabled = `server_settings$tableau_integration_enabled`"
  if (is.null(server_settings$tableau_integration_enabled)) {
    "!DEBUG Tableau.IntegrationEnabled is not present within server settings. This Connect server does not support the feature (`server_settings$version`)"
    # Problem: Feature Flag not available, Connect version (server_settings$version) does not support feature
    message_contents <- paste0(
      message_contents,
      paste0("Problem: Tableau Integration Feature Flag is not available on the RStudio Connect server (v.", server_settings$version, "). Please upgrade to the latest version"),
      sep = "\n"
    )
    # Resolve: Upgrade to latest RStudio Connect version
  } else if (!server_settings$tableau_integration_enabled) {
    message_contents <- paste0(
      message_contents,
      "Problem: Tableau Integration has been disabled on the RStudio Connect server. Please ask your administrator to set Tableau.TableauIntegrationEnabled = true within RStudio Connect config file",
      sep = "\n"
    )
    # Problem: Feature Flag has been disabled
    # Resolve: Ask administrator to set Tableau.TableauIntegrationEnabled = true within RStudio Connect config file
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
