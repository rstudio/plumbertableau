# This is a function that takes a router, and returns a function that operates
# on the OpenAPI spec that's generated for that router. We then call
# plumber::pr_set_api_spec() on the function that this returns.
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

    # Remove paths from spec so they don't show in UI
    spec$paths[["/"]] <- NULL
    spec$paths[["/setup"]] <- NULL
    spec$paths[["/user"]] <- NULL
    spec$paths[["/help"]] <- NULL

    # We have different consumers of the description, so we'll split them apart..
    spec$info$user_description <- paste0(
      "### Description\n",
      spec$info$description,
      sep = "\n"
    )

    # Provide additional context in the description field for the OpenAPI documentation.
    warnings <- warning_message()
    if (!rlang::is_null(warnings)) {
      spec$info$description <- paste0(
        "### Warnings",
        "\n",
        "#### The following item(s) need to be resolved before your API will be accessible from Tableau:",
        "\n\n---\n\n",
        warnings,
        sep = "\n"
      )
    } else {
      spec$info$description <- paste0(
        "### Description\n",
        spec$info$description,
        "\n",
        "#### Use the following links to setup and use your Tableau Analytics Extension.",
        "\n",
        "* [Use your analytics extension from Tableau](../)",
        "\n",
        "* [Configure Tableau to use your analytics extension](../setup)",
        "\n",
        "* [Read up on plumbertableau, Tableau, and RStudio Connect](../help)",
        sep = "\n"
      )
    }

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

  # Create argument map detailing how Tableau arguments relate to the arguments
  # defined for the plumbertableau extension function
  arg_map <- Reduce(rbind, lapply(args, as.data.frame))
  arg_map$`arg name` <- names(args)
  arg_map$`tableau name` <- names(arg_list)
  arg_map <- arg_map[,c("arg name", "tableau name", "type", "desc", "optional")]
  names(arg_map) <- c("Arg name", "Tableau name", "Type", "Description", "Optional")

  list(description = paste0(
    markdown::markdownToHTML(
    text = "### Tableau Request
This is a mock Tableau request. Tableau sends a JSON request formatted like the following JSON. Tableau doesn't provide named arguments and instead assigns each argument `_arg1`, `_arg2`, ... , `_argN`.

The provided JSON is a simple template based on the described arguments and return values. A more comprehensive mock request can be generated with `mock_tableau_request()`.",
    fragment.only = TRUE),
knitr::kable(arg_map, format = "html")
),
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
