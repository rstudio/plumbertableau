tableau_openapi <- function(pr) {
  "!DEBUG Creating Tableau OpenAPI Spec"
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
      required = !args[[arg_name]]$optional,
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
                 required = names(arg_list)[unlist(lapply(arg_list, function(arg) arg$required))],
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


