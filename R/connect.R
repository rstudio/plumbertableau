#' Class representing a Connect API client
#'
#' @name Client
#'
#' @section Details:
#'
#' This class allows a user to interact with a Connect server via the Connect
#' API. Authentication is done by providing an API key.
#'
#' @importFrom utils capture.output
#'
#' @family R6 classes
Client <- R6::R6Class( # nolint
  "Client",
  public = list(
    server = NULL,
    api_key = NULL,
    minimum_server_version = "1.8.8.3",
    initialize = function(server, api_key) {
      self$server <- server
      self$api_key <- api_key

      private$validate()
    },
    print = function(...) {
      cat("RStudio Connect API Client: \n")
      cat("  Server: ", self$server, "\n", sep = "")
      cat("  API Key: ", paste0(strrep("*", 11), substr(
        self$api_key, nchar(self$api_key) - 3, nchar(self$api_key)
      )), "\n", sep = "")
      invisible(self)
    },
    GET = function(path, ..., writer = httr::write_memory(), parser = "text") {
      url <- paste0(self$server, "/__api__", path)
      res <- httr::GET(
        url,
        private$add_auth(),
        writer,
        ...
      )
      private$raise_error(res)
      httr::content(res, as = parser)
    },
    content = function() {
      results <- self$GET("/v1/content", query = list(include = "tags,owner"))
      jsonlite::fromJSON(results, simplifyDataFrame = T)
    },
    server_settings = function() {
      self$GET("/server_settings", parser = "parsed")
    }
  ),
  private = list(
    validate = function() {
      api_key <- self$api_key
      server <- self$server

      if (is.null(api_key) || is.na(api_key) || nchar(api_key) == 0) {
        stop("ERROR: Please provide a valid API key")
      }

      if (is.null(server) || is.na(server) || nchar(server) == 0) {
        stop("ERROR: Please provide a valid server URL")
      }

      if (is.null(httr::parse_url(server)$scheme)) {
        stop(glue::glue(
          "ERROR: Please provide a protocol (http / https). You gave: {server}"
        ))
      }

      settings <- tryCatch({
        self$server_settings()
      },
      error = function(e) {
        message(e)
        stop(
          glue::glue(
            "ERROR: Unable to connect to RStudio Connect at {server}"
          )
        )
      }
      )

      if (compareVersion(settings$version, self$minimum_server_version) < 0) {
        stop(
          glue::glue("ERROR: Requires RStudio Connect server version >= ",
                     self$minimum_server_version,
                     ", current version ",
                     settings$version)
        )
      }
    },
    raise_error = function(res) {
      if (httr::http_error(res)) {
        err <- sprintf(
          "%s request failed with %s",
          res$request$url,
          httr::http_status(res)$message
        )
        message(capture.output(str(httr::content(res))))
        stop(err)
      }
    },
    add_auth = function() {
      httr::add_headers(Authorization = paste0("Key ", self$api_key))
    }
  )
)

#' Create a connection to RStudio Connect
#'
#' Creates a connection to RStudio Connect using the server URL and an api key.
#'
#' @param server The server URL for accessing RStudio Connect. Defaults to
#'   environment variable CONNECT_SERVER
#' @param api_key The API key to authenticate with RStudio Connect. Defaults
#'   to environment variable CONNECT_API_KEY
#'
#' @return An Client object
#'
#' @rdname connect
#'
#' @export
connect <- function(server = Sys.getenv("CONNECT_SERVER", NA_character_),
                    api_key = Sys.getenv("CONNECT_API_KEY", NA_character_)) {
  Client$new(server = server, api_key = api_key)
}
