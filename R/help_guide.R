globalVariables(names(htmltools::tags))

create_help <- function(pr) {
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
    render_help(content_path, pr)
  }
}

render_help <- function(path, pr) {
  apiSpec <- pr$getApiSpec()
  title <- apiSpec$info$title
  version <- apiSpec$info$version
  desc <- markdown::markdownToHTML(text = apiSpec$info$user_description,
                                    fragment.only = TRUE)

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

  body_content <- htmltools::tagList(
    tags$h3(
      class="subtitle",
      "Additional details about plumbertableau, Tableau, and RStudio Connect"
    ),
    tags$div(
      class = "padded-fully",
      htmltools::HTML(markdown::markdownToHTML(
        text = help_text()), fragment.only = TRUE
      )
    )
  )
  
  as.character(htmltools::htmlTemplate(
    system.file("template/index.html", package = "plumbertableau", mustWork = TRUE),
    title_desc = title_desc,
    body_content = body_content
  ))
}

help_text <- function() {
"
[Tableau](https://www.tableau.com) is a leading visual analytics platform that lets its users investigate, understand, and report on data. R is a programming language for statistics, data analysis, and visualization.

plumbertableau lets you call external R code from Tableau workbooks via [Tableau Analytics Extensions](https://tableau.github.io/analytics-extensions-api/). You achieve this by writing a plumbertableau extension, which is a [Plumber](https://www.rplumber.io/) API with some extra annotations. plumbertableau uses these annotations to correctly serve requests from Tableau, as well as dynamically generate documentation, copy-and-pasteable Tableau code, and setup instructions.

plumbertableau extensions are most easily used with [RStudio Connect](https://www.rstudio.com/products/connect/), which lets you host and manage any number of Tableau extensions along with other content types.

Before you use the extension in Tableau, Tableau needs to be able to access it. You can find instructions on how to do that, and information on publishing extensions to RStudio Connect, in **[Publishing plumbertableau Extensions to RStudio Connect](publishing-extensions.html)**. This document will walk you through publishing extensions to RStudio Connect and setting up a Connect server as an extension in Tableau.

## Learn More

- [Writing plumbertableau Extensions in R](r-developer-guide.html)
- [Publishing plumbertableau Extensions to RStudio Connect](tableau-configutation.html)
- [Using plumbertableau Extensions in Tableau](tableau-developer-guide.html)
"
}
