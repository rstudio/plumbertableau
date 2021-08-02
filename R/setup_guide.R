globalVariables(names(htmltools::tags))

create_setup_instructions <- function(pr) {
  cached_instructions <- NULL

  function(req, res) {
    "!DEBUG `write_log_message(req, res, 'Generating Tableau Setup Instructions')"
    if (is.null(cached_instructions)) {
      # Caching works b/c R is restarted when the vanity path changes on RStudio Connect
      cached_instructions <<- render_setup_instructions(req$content_path, pr)
    }

    cached_instructions
  }
}

render_setup_instructions <- function(path, pr) {
  connect_server <- Sys.getenv("CONNECT_SERVER")
  server_domain <- urltools::domain(connect_server)
  server_port <- urltools::port(connect_server)
  warnings <- warning_message()

  if (rlang::is_na(server_port)) {
    server_scheme <- urltools::scheme(connect_server)
    if (rlang::is_na(server_scheme)) {
      # Do nothing
    } else if (server_scheme == "http") {
      server_port <- 80
    } else if (server_scheme == "https")
      server_port <- 443
  }
  apiSpec <- pr$getApiSpec()
  desc <- markdown::markdownToHTML(text = apiSpec$info$user_description,
                                    fragment.only = TRUE)
  title <- apiSpec$info$title
  version <- apiSpec$info$version
  body_content <- "body_content not generated"

  title_desc <- htmltools::tagList(
    tags$h1(
      class="padded-fully title",
      title,
      if (!is.null(version)) paste0("(v", version, ")")
    ),
    tags$div(class = "api-desc",
      tags$div(
        class="padded-flat-top",
        htmltools::HTML(desc)
      )
    )
  )

  if (!rlang::is_null(warnings)) {
    body_content <- htmltools::tagList(
      tags$h3(
        class="warning",
        "Warning: The following item(s) need to be resolved before your API will be accessible from Tableau!"
      ),
      tags$div(
        class="padded-flat-top",
        htmltools::HTML(markdown::markdownToHTML(text = warnings, fragment.only = TRUE))
      )
    )
  } else {
    body_content <- htmltools::tagList(
      tags$h3(
        class="subtitle",
        "Configure Tableau to access this extension"
      ),
      tags$div(
        class="padded-flat-top",
        tags$h3("If you are using Tableau Server or Tableau Online:"),
        tags$ol(
          tags$li("Using an administrative account, login to Tableau Server/Online"),
          tags$li("Navigate to Settings, then Extensions"),
          tags$li("Under the heading 'Analytics Extensions', select 'Enable analytics extension for site"),
          tags$li("Create a new connection and select the connection type of 'Analytics Extensions API'"),
          tags$li("Select if you want to use SSL"),
          tags$li("Enter the information for your RStudio Connect Server:"),
          tags$div(
            class="values",
            tags$div(
              tags$span(class="emphasized", "Host:"),
              server_domain
            ),
            tags$div(
              tags$span(class="emphasized", "Port:"),
              server_port
            )
          ),
          tags$li("Select 'Sign in with a username and password' and enter the credentials:"),
          tags$div(
            class="values",
            tags$div(
              tags$span(class="emphasized", "Username:"),
              "rstudio-connect"
            ),
            tags$div(
              tags$span(class="emphasized", "Password:"),
              tags$span(class="italic", "any valid API key from RStudio Connect")
            )
          ),
          tags$li("Create / Save changes")
        )
      ),
      tags$div(
        class="padded-flat-top",
        tags$h3("If you are using Tableau Desktop:"),
        tags$ol(
          tags$li("Navigate to Help, Settings and Performance, Manage Analytics Extension Connection..."),
          tags$li("Select 'TabPy/External API'"),
          tags$li("Enter the information for your RStudio Connect Server:"),
          tags$div(
            class="values",
            tags$div(
              tags$span(class="emphasized", "Host:"),
              server_domain
            ),
            tags$div(
              tags$span(class="emphasized", "Port:"),
              server_port
            )
          ),
          tags$li("If desired, select 'Sign in with a username and password' and enter the credentials:"),
          tags$div(
            class="values",
            tags$div(
              tags$span(class="emphasized", "Username:"),
              "rstudio-connect"
            ),
            tags$div(
              tags$span(class="emphasized", "Password:"),
              tags$span(class="italic", "any valid API key from RStudio Connect")
            )
          ),
          tags$li("Select whether to Require SSL"),
          tags$li("Save changes")
        )
      )
    )
  }

  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    title_desc = title_desc,
    body_content = body_content
  ))
}
