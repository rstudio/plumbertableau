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
  url <- paste0(server, "/__api__/server_settings")
  server_settings <- NULL
  tryCatch (
    {
       "!DEBUG Sending GET request @ `url`"
      response <- httr::GET(
        url,
        httr::add_headers(Authorization = paste0("Key ", api_key)),
        httr::write_memory()
      )
      "!DEBUG GET response has returned `http_status(response)$category`, `http_status(response)$reason`, `http_status(response)$message`"
      if (httr::http_error(response)) {
        "!DEBUG GET response returned an error"
        # NEED TO enhance the message below to include next steps...
        message_contents <- paste0(
          message_contents,
          paste0("Problem: API request to ", server, " failed. Response: ", http_status(response)$reason, ", ", http_status(response)$message, "."),
          sep = "\n"
        )
        # Problem: request failed, with error in response
        # Resolve: as appropriate...
      } else {
        server_settings <- httr::content(res, as="text")
        "!DEBUG GET response was successful. Server settings = `server_settings`"
      }
    },
    error = function(err) {
      "!DEBUG GET response threw an exception: `err`"
      # Problem: request failed, with error = err
      message_contents <- paste0(
        message_contents,
        paste0("Problem: API request to ", server, " failed. Error: ", err),
        sep = "\n"
      )
      return (NULL)
      # Resolve: If using self-signed certificates, define PLUMBERTABLEAU_USE_HTTP = TRUE if able..
    }
  )
  "!DEBUG message_contents after GET request: `message_contents`"
  if (!is.null(message_contents)) {
    return (message_contents)
  }

  # Does this installation support Tableau Extensions
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
