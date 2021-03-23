tableau_openapi <- function(pr, path) {
  spec <- pr$getApiSpec()
  route_info <- extract_route_info(path, pr)
  spec_paths <- names(spec$paths)
  # Identify Tableau routes from route_info (these are the only routes from route_info)
  tableau_routes <- unlist(lapply(route_info, function(route) route[["path"]]))

  for (path in spec_paths) {
    if (path %in% tableau_routes) {
      spec$paths[[path]][["post"]][["requestBody"]] <- build_tableau_spec(route_info[tableau_routes == path][[1]])
    }
  }

  # Return OAS as a list
  spec
}

build_tableau_spec <- function(route_attrs) {
  # Extract expected arguments supplied in Tableau request
  args <- route_attrs$arg_spec
  # Convert the argument descriptions to be OAS compliant
  arg_list <- lapply(names(args), function(arg_name) {
    list(
      type = "array",
      description = arg_name,
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
                 required = "_arg1",
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


