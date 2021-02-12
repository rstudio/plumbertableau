# Tableau POST spec
tableau_spec <- yaml::read_yaml(system.file("openapi/tableau-evaluate-spec.yaml",
                                            package = "plumbertableau"))


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


