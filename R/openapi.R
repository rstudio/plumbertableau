tableau_openapi <- function(pr) {
  function(spec) {
    route_info <- extract_route_info(pr)
    spec_paths <- names(spec$paths)
    # Identify Tableau routes from route_info (these are the only routes from route_info)
    tableau_routes <- unlist(lapply(route_info, function(route) route[["path"]]))

    for (path in spec_paths) {
      if (path %in% tableau_routes) {
        spec$paths[[path]][["post"]][["requestBody"]] <- build_tableau_spec(route_info[tableau_routes == path][[1]])
      }
    }

    # Remove / from spec so it doesn't show in UI
    spec$paths[["/"]] <- NULL

    # Provide additional context in the description field. This is also visible
    # in the user guide
    warnings <- warning_message()
    if (!rlang::is_null(warnings)) {
      spec$info$description <- paste0(
        "### Warnings\n",
        warnings,
        "\n\n***\n### Info\n",
        info_message(),
        "\n\n***\n### Setup\n",
        extension_setup(),
        "\n\n***\n### Description\n",
        spec$info$description
      )
    } else {
      spec$info$description <- paste0(
        "### Info\n",
        info_message(),
        "\n\n***\n### Setup\n",
        extension_setup(),
        "\n\n***\n### Description\n",
        spec$info$description
      )
    }



    # Add reference back to Tableau user guide
    spec$externalDocs <- list(
      description = "Tableau Usage Instructions",
      url = "/"
    )

    # Return OAS as a list
    spec
  }
}

build_tableau_spec <- function(route_attrs) {
  # Extract expected arguments supplied in Tableau request
  args <- route_attrs$arg_spec
  # Convert the argument descriptions to be OAS compliant
  arg_list <- lapply(names(args), function(arg_name) {
    list(
      type = "array",
      description = ifelse(args[[arg_name]]$desc == "", arg_name, paste0(arg_name, ": ", args[[arg_name]]$desc)),
      items = list(
        type = json_type(args[[arg_name]]$type)
      )
    )
  })

  names(arg_list) <- paste0("_arg", 1:length(arg_list))

  list(description = "Tableau Request",
       required = TRUE,
       content = list(
         `application/json` = list(
           schema = list(
             type = "object",
             required = c("script", "data"),
             properties = list(
               script = list(
                 type = "string",
                 description = "Path to desired endpoint",
                 example = route_attrs$path
               ),
               data = list(
                 type = "object",
                 required = as.list(names(arg_list)[unlist(lapply(args, function(arg) !arg$optional))]),
                 properties = arg_list
               )
             )
           )
         )
       )
  )
}

json_type <- function(type) {
  switch(
    type,
    character = "string",
    integer = "number",
    numeric = "number",
    logical = "boolean",
    double = "number"
  )
}


# Generate an informational message based on the execution context of the extension
warning_message <- function() {
  message_contents <- NULL
  # RStudio Connect Details
  # Provide messaging if RSC:
  #  * Is a version that doesn't support Tableau Extensions
  #  * Isn't configured to support Tableau Extensions
  #  * Doesn't have Server.Address configured
  # TODO: Replace this with proper logic once available

  # Server.Address
  connect_server <- Sys.getenv("CONNECT_SERVER")

  # Does this installation support Tableau Extensions
  connect_support <- Sys.getenv("RSC_TABLEAU")

  # RStudio Connect version
  connect_version <- Sys.getenv("RSC_VERSION")

  if (connect_version != "1.9.0") {
    message_contents <- paste(message_contents,
                              "* **This version of RStudio Connect does not support Tableau Analytics Extension APIs. Please upgrade RStudio Connect to at least version 1.9.x**",
                              sep = "\n")
  }
  if (!rlang::is_true(as.logical(connect_support))) {
    message_contents <- paste(message_contents,
                              "* This installation of RStudio Connect does not currently support Tableau Analytics Extension APIs.",
                              sep = "\n")
  }

  if (connect_server == "") {
    message_contents <- paste(message_contents,
                              "* The `Server.Address` property isn't configured for this installation of RStudio Connect.",
                              sep = "\n")
  }

  # Send warning message to the console if any of the above are TRUE
  if (!rlang::is_null(message_contents)) {
    message_contents <- paste(
      message_contents,
      "\n\n#### Please reach out to your RStudio Connect administrator",
      sep = "\n"
    )

    rlang::warn(stringi::stri_replace_all(message_contents, regex = "\\* ?", replacement = ""), .frequency = "once", .frequency_id = "rsc_warning")
  }

  message_contents
}


info_message <- function() {
  message_contents <- NULL
  if (stringi::stri_detect(Sys.getenv("DEBUGME"), fixed = "plumbertableau")) {
    message_contents <- paste(message_contents,
                              "* Debugging is **on**.
  * To disable it please remove the `DEBUGME` environment variable or set it to a value that does not include 'plumbertableau'.",
  sep = "\n")
  } else {
    message_contents <- paste(message_contents,
                              "* Debugging is **off**.
  * To enable it please set the environment variable `DEBUGME` to include 'plumbertableau'.",
  sep = "\n")
  }

  message_contents
}


# Generate message for extension setup
extension_setup <- function() {
  # TODO: Dynamically describe the actual values for Host and Port based on
  # executing environment
  "<details>
  <summary>Tableau setup instructions</summary>

#### Tableau Server / Tableau Online
  1. Using an administrative account, login to Tableau Server
  2. Navigate to Settings, then Extensions
  3. At the bottom of the page, choose to *Enable analytics extensions for site*
  4. Create a new connection and select the Connection Type of 'TapPy/Analytics Extension'
  5. Select if you want to use SSL and enter the server Host and Port for your RStudio Connect server
  6. If desired, select 'Sign in with a username and password'. The username is 'rstudio-connect' and the password is any valid API key from RStudio Connect
  8. Save changes

#### Tableau Desktop
  1. Navigate to Help, Settings and Performance, Manage Analytics Extension Connection...
  2. Select 'TabPy/External API'
  3. Set Server and Port to the address and port of the server running the API
  4. If desired, select 'Sign in with a username and password'. The username is 'rstudio-connect' and the password is any valid API key from RStudio Connect
  5. Select whether to Require SSL
  6. Save changes
  </details>"
}
