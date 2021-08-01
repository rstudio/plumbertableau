# MUST HAVE
# TODO: URL param default values should be shown

# NICE TO HAVE
# TODO: URL param values should be coerced to type (if possible?)

library(fontawesome)

globalVariables(names(htmltools::tags))

#' @importFrom htmltools tags
create_overview <- function(pr) {
  function(req, res) {
    "!DEBUG `write_log_message(req, res, 'Generating Tableau Analytical Extension Overview')"
    # Parse the endpoint path from header on RStudio Connect
    content_path <- NULL
    if (!rlang::is_null(req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL)) {
      base_url <- req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL
      rsc_root <- Sys.getenv("CONNECT_SERVER")
      content_path <- gsub(rsc_root,
                           "",
                           base_url,
                           fixed = TRUE
      )

      # If the path requested is not root (/), strip it from the content path
      if (req$PATH_INFO != "/") {
        content_path <- gsub(req$PATH_INFO,
                             "",
                             content_path,
                             fixed = TRUE
        )
      }

      # Ensure path starts with '/'
      if (!stringi::stri_startswith(content_path, fixed = "/")) content_path <- paste0("/", content_path)
    }
    render_overview(content_path, pr)
  }
}

render_overview <- function(path, pr) {
  warnings <- warning_message()

  apiSpec <- pr$getApiSpec()
  title <- apiSpec$info$title
  version <- apiSpec$info$version

  # Strip description of links to other pages
  desc <- markdown::markdownToHTML(text = strip_md_links(apiSpec$info$description),
                                    fragment.only = TRUE)
  ui <- htmltools::tagList(
    tags$header(
      tags$div(
        class="nav",
        tags$div(
          class="main-menu",
          tags$ul(
            tags$li(
              tags$a(
                href = "./",
                tags$div(
                  class="menuitem",
                  fa("home")
                ),
                tags$span(
                  class="nav-text",
                  "Introduction to Tableau Analytics Extensions from RStudio Connect"
                )
              )
            ),
            tags$li(
              tags$a(
                href = "./setup",
                tags$div(
                  class="menuitem",
                  fa("cogs")
                ),
                tags$span(
                  class="nav-text",
                  "Configure Tableau to use your analytics extension"
                )
              )
            ),
            tags$li(
              tags$a(
                href = "./user",
                tags$div(
                  class="menuitem",
                  fa("chart-area")
                ),
                tags$span(
                  class="nav-text",
                  "Call your analytics extension from Tableau"
                )
              )
            ),
            tags$li(
              tags$a(
                href = "./__docs__/",
                tags$div(
                  class="menuitem",
                  fa("info-circle")
                ),
                tags$span(
                  class="nav-text",
                  "View your extension's Open API documentation"
                )
              )
            ),
            tags$li(
              tags$a(
                tags$div(
                  class="menuitem",
                  fa("question-circle")
                ),
                tags$span(
                  class="nav-text",
                  "View the plumbertableau package documentation"
                )
              )
            )
          )
        )
      ),
      tags$div(
        class="title_desc_container",
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
    ),
    tags$main(
      tags$h3(
        class="subtitle",
        "Use your analytics extension from Tableau"
      ),
      tags$div(class = "padded-fully",
        tgs$h4(
          "Some kind of good overview here.. Of how you use Tableau extensions from RStudio Connect"
        )
      )
    )
  )

  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    content = ui
  ))
}
