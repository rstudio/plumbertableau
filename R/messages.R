# Gathers environment data about the extension's execution environment and
# returns a warning message about factors that would prevent the extension
# from running correctly, or receiving and responding to requests from
# Tableau.
warning_message <- function() {

  # If not running on RSC, return NULL, otherwise perform checks;
  if (!check_rstudio_connect()) {
    return (NULL)
  }

  message_contents <- NULL
  message_count <- 0

  server <- Sys.getenv("CONNECT_SERVER", NA_character_)
  "!DEBUG Environment Variable CONNECT_SERVER = `server`"

  if (is.na(server) || server == "") {
    "!DEBUG Problem: CONNECT_SERVER not defined within environment variables"

    message_contents <- paste0(
      message_contents,
      paste0("### The environment variable *CONNECT_SERVER* is not defined.",
        "\n",
        "\nPossible Solutions:",
        "\n",
        "\n-    Have your system administrator confirm *Applications.DefaultServerEnv* is enabled and that *Server.Address* has been defined within the *rstudio-connect.gcfg* file on the RStudio Connect server.",
        "\n-    Use the application settings for your content within the RStudio Connect dashboard to define the *CONNECT_SERVER* environment variable. It should be set to a reachable https or http address for the server."
        ),
      sep = "\n\n---\n\n"
    )
    message_count <- message_count + 1
  } else if (is.null(httr::parse_url(server)$scheme)) {
    "!DEBUG Problem: Environment Variable CONNECT_SERVER does not specify the protocol (https:// or http://)."

    message_contents <- paste0(
      message_contents,
      paste0("### Environment Variable *CONNECT_SERVER* (value = *", server, "* ) does not specify the protocol (*https://* or *http://*).",
        "\n",
        "\nPossible Solutions:",
        "\n",
        "\n-    Have your system administrator confirm that *Server.Address* has been configured with the proper format within the *rstudio-connect.gcfg* file on the RStudio Connect server.",
        "\n-    Use the application settings for your content within the RStudio Connect dashboard to define the *CONNECT_SERVER* environment variable with the proper protocol."
        ),
      sep = "\n\n---\n\n"
    )
    message_count <- message_count + 1
  }

  api_key = Sys.getenv("CONNECT_API_KEY", NA_character_)
  # NOTE: Do not output the API KEY value!!!

  if (is.na(api_key) || api_key == "") {
    "!DEBUG Problem: CONNECT_API_KEY not defined within environment variables"

    message_contents <- paste0(
      message_contents,
      paste0("### The environment variable *CONNECT_API_KEY* is not defined.",
        "\n",
        "\nPossible Solutions:",
        "\n",
        "\n- Have your administrator enable the *Applications.DefaultAPIKeyEnv* setting within the *rstudio-connect.gcfg* file on the RStudio Connect server.",
        "\n- Create an *API KEY* yourself and use the application settings for your content within the RStudio Connect dashboard to define the *CONNECT_API_KEY* variable with the *API KEY* value."
        ),
      sep = "\n\n---\n\n"
    )
    message_count <- message_count + 1
  }

  if (message_count > 0) {
    return (message_contents)
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
  if (!is.na(server)) {
    last_char <- substr(server, nchar(server), nchar(server))
    if (!is.na(last_char) && last_char != "/") {
      server <- paste0(server, "/")
    }
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

      return (list(
        success=FALSE,
        message=paste0("### API request to ", server, " has failed with error:",
            "\n", err,
            "\n",
            "\nPossible Solutions:",
            "\n",
            "\n-    If you have specified an API_KEY, confirm it is valid.",
            "\n-    Confirm there is connectivity between the server itself and the address assigned to it: ", server, ".",
            "\n-    If using HTTPS along with self-signed certificates, you may need to allow the plumbertableau package to use HTTP instead, ",
            "by setting the environment variable *PLUMBERTABLEAU_USE_HTTP* to *TRUE* within the RStudio Connect application settings."
        )
      ))
    }
  )
  if (!result$success) {
    "!DEBUG Detected that an error has been returned from exception: `result`"

    message_contents <- paste0(
      message_contents,
      result$message,
      sep = "\n\n---\n\n"
    )
    message_count <- message_count + 1
  } else {
    "!DEBUG GET response has returned `httr::http_status(result$response)$category`, `httr::http_status(result$response)$reason`, `httr::http_status(result$response)$message`"

    if (httr::http_error(result$response)) {
      message_contents <- paste0(
        message_contents,
        paste0("### API request to ", server, " failed. ",
        "\n-    Response: ", httr::http_status(result$response)$reason, ", ", httr::http_status(result$response)$message,
        "\n",
        "\nPossible Solution:",
        "\n",
        "\nDiagnose connectivity or access issue."
        ),
        sep = "\n\n---\n\n"
      )
      message_count <- message_count + 1

    } else {
      "!DEBUG GET response was successful."
      server_settings <- httr::content(result$response, as="parsed")
    }
  }

  if (message_count > 0) {
    return (message_contents)
  }

  # Does this installation support Tableau Extensions?
  "!DEBUG server_settings$tableau_integration_enabled = `server_settings$tableau_integration_enabled`"
  if (is.null(server_settings$tableau_integration_enabled)) {
    "!DEBUG Tableau.IntegrationEnabled is not present within server settings. This Connect server does not support the feature (`server_settings$version`)"

    message_contents <- paste0(
      message_contents,
      paste0("### Tableau Integration Feature Flag is not available on the RStudio Connect server. Current server is version: ", server_settings$version,
        "\n",
        "\nPossible Solution:",
        "\n",
        "\nPlease upgrade to the latest version of RStudio Connect"
        ),
      sep = "\n\n---\n\n"
    )

  } else if (!server_settings$tableau_integration_enabled) {

    message_contents <- paste0(
      message_contents,
      paste0("### Tableau Integration has been disabled on the RStudio Connect server",
        "\n",
        "\nPossible Solution:",
        "\n",
        "\nPlease ask your administrator to set *Tableau.TableauIntegrationEnabled* = *true* within *rstudio-connect.gcfg* file on the RStudio Connect server."
        ),
      sep = "\n\n---\n\n"
    )
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
      "Verbose logging is on. To disable it please remove the `DEBUGME` environment variable or set it to a value that does not include `plumbertableau`.",
      sep = "\n\n"
    )
  } else {
    message_contents <- paste0(
      message_contents,
      "Verbose logging is off. To enable it please set the environment variable `DEBUGME` to include `plumbertableau`.",
      sep = "\n\n"
    )
  }

  message_contents
}
