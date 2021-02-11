#* Generate an OpenAPI file that matches Tableau's request specification
tableau_openapi <- function(spec) {

}


#* Tableau POST spec
list(
  description = "Tabluea Data",
  required = TRUE,
  content = list(
    `application/json` = list(
      schema = list(
        type = "object",
        properties = list(
          script = list(
            type = "string",
            title = "Path to desired endpoint",
            example = "/foo"
          ),
          data = list(
            # The type of data is a JSON object
          )
        )
      )
    )
  )
)
