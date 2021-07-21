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
    orig_server = NULL,
    api_key = NULL,
    allow_downgrade = FALSE,
    url_downgraded = FALSE,
    initialize = function(server, api_key, allow_downgrade) {
      self$server <- server
      self$orig_server <- self$server
      self$api_key <- api_key
      self$allow_downgrade <- allow_downgrade
      private$validate()
      private$test_connection()
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
    server_settings = function() {
      self$GET("/server_settings", parser = "parsed")
    },
    vanity = function(appGUID) {
      self$GET(paste0("/v1/content/", appGUID, "/vanity"), parser = "parsed")
    },
    content = function(appGUID) {
      results <- self$GET(paste0("/v1/content/", appGUID))
      jsonlite::fromJSON(results, simplifyDataFrame = T)
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
    },
    test_connection = function() {
      original_exception <- NULL
      downgraded_exception <- NULL
      success <- FALSE
      tryCatch({
        self$server_settings()
        success <- TRUE
      },
      error = function(e) {
        original_exception <- e
        success <- FALSE
      }
      )

      # if we don't succeed and we're able to downgrade a https connection, then try it
      if (!success && self$allow_downgrade && httr::parse_url(self$orig_server)$scheme == "https") {
        self$server <- sub("https://", "http://", self$orig_server)
        tryCatch({
          self$server_settings()
          success <- TRUE
          url_downgraded <- TRUE
          cat("WARNING: Using http:// to access the Connect server.")
        },
        error = function(e) {
          downgraded_exception <- e
          success <- FALSE
        }
        )
      }

      if (!success) {
        if (!is.null(downgraded_exception)) {
          message(original_exception)
          stop(
            glue::glue(
              "ERROR: Unable to connect to RStudio Connect at {self$orig_server}"
            )
          )
        } else {
          message(original_exception)
          message(downgraded_exception)
          stop(
            glue::glue(
              "ERROR: Unable to connect to RStudio Connect at {self$orig_server} or {self$downgraded_server}"
            )
          )
        }
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
                    api_key = Sys.getenv("CONNECT_API_KEY", NA_character_),
                    allow_downgrade = FALSE) {
  Client$new(server = server, api_key = api_key, allow_downgrade = allow_downgrade)
}
