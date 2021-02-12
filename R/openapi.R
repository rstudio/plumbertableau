#* Generate an OpenAPI file that matches Tableau's request specification
tableau_openapi <- function(spec) {

}


#* Tableau POST spec
tableau_spec <- list(
  description = "Tableau Request",
  required = TRUE,
  content = list(
    `application/json` = list(
      schema = list(
        type = "object",
        required = c("script", "data"),
        properties = list(
          script = list(
            type = "string",
            title = "Path to desired endpoint",
            example = "/foo"
          ),
          data = list(
            type = "object",
            required = "_arg1",
            properties = list(
              `_arg1` = list(
                type = "array",
                title = "First object passed from Tableau",
                example = c(1)
              ),
              `_argN` = list(
                type = "array",
                title = "Other optional objects passed from Tableau",
                example = c("a")
              )
            )
          )
        )
      )
    )
  )
)
