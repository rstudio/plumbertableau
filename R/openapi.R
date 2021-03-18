# Tableau POST spec
tableau_spec <- list(description = "Tableau Request",
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
                               example = "/foo"
                             ),
                             data = list(
                               type = "object",
                               required = "_arg1",
                               properties = list(
                                 `_arg1` = list(
                                   type = "array",
                                   description = "First object passed from Tableau", example = 1:3),
                                 `_argN` = list(
                                   type = "array",
                                   description = "Other optional objects passed from Tableau",
                                   example = c("a", "b", "c")
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
)


#* Generate an OpenAPI file that matches Tableau's request specification
tableau_openapi <- function(spec) {
  for (i in 1:length(spec$paths)) {
    if (!is.null(spec$paths[[i]][["post"]])) {
      # For every post request, attach a description of the Tableau requestBody
      # TODO: Is there a way to only identify post endpoints that are Tableau
      # specific?
      spec$paths[[i]][["post"]][["requestBody"]] <- tableau_spec
    }
  }

  spec
}


