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
    error_encountered = FALSE,
    failure_messages = list(),
    initialize = function(server, api_key, allow_downgrade) {
      "!DEBUG New Connect object"
      self$server <- server
      self$orig_server <- self$server
      self$api_key <- api_key
      self$allow_downgrade <- allow_downgrade
    },
    validate = function() {
      "!DEBUG Connect object validate called"
      validate_parameters_result <- private$validate_parameters()
      "!DEBUG private$validate_parameters() resulted in `validate_parameters_result`"
      test_connection_result <- private$test_connection()
      "!DEBUG private$test_connection() resulted in `test_connection_result`"
      validate_parameters_result && test_connection_result
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
      if (!private$check_for_http_error(res)) {
        httr::content(res, as = parser)
      }
      NULL
    },
    server_settings = function() {
      self$GET("/server_settings", parser = "parsed")
    },
    vanity = function(appGUID) {
      self$GET(paste0("/v1/content/", appGUID, "/vanity"), parser = "parsed")
    },
    content = function(appGUID) {
      results <- self$GET(paste0("/v1/content/", appGUID))
      if (!is.null(results)) {
        jsonlite::fromJSON(results, simplifyDataFrame = T)
      }
      NULL
    }
  ),
  private = list(
    validate_parameters = function() {
      "!DEBUG Connect validate_parameters called"
      api_key <- self$api_key
      server <- self$server
      self$failure_messages <- list()

      if (is.null(api_key) || is.na(api_key) || nchar(api_key) == 0) {
        self$failure_messages.append("ERROR: API key is missing or invalid.")
      }

      if (is.null(server) || is.na(server) || nchar(server) == 0) {
        self$failure_messages.append("ERROR: Server URL is missing or invalid")
      }

      if (is.null(httr::parse_url(server)$scheme)) {
        self$failure_messages.append("ERROR: Protocol missing in server URL (http / https). You gave: {server}")
      }
      "!DEBUG validate_parameters has errors? `length(self$failure_messages) > 0`"
      for (i in self$failure_messages) {
        "!DEBUG validate_parameters error: `i`"
      }
      self$error_encountered <- length(self$failure_messages) > 0
      !self$error_encountered
    },
    test_connection = function() {
      "!DEBUG Connect test_connection called"
      self$error_encountered <- FALSE
      original_exception <- NULL
      downgraded_exception <- NULL
      self$failure_messages <- list()
      tryCatch(
        {
          "!DEBUG attempting connection with server at `self$server`"
          self$server_settings()
          self$error_encountered <- FALSE
          "!DEBUG connection successful"
          return(TRUE)
        },
        error = function(err) {
          "!DEBUG connection failed: `err`"
          original_exception <- err
          self$error_encountered <- TRUE
        }
      )
      "!DEBUG After attempting connection.. success?: `!self$error_encountered`"

      # if we don't succeed and we're able to downgrade a https connection, then try it
      if (self$error_encountered && self$allow_downgrade && httr::parse_url(self$orig_server)$scheme == "https") {
        self$server <- sub("https://", "http://", self$orig_server)
        "!DEBUG attempting downgrading connection with server at `self$server`"
        tryCatch(
          {
            self$server_settings()
            "!DEBUG connection successful"
            self$error_encountered <- FALSE
            url_downgraded <- TRUE
            self$failure_messages <- list()
            cat("WARNING: Using http:// to access the Connect server.")
            return(TRUE)
          },
          error = function(err) {
            "!DEBUG connection failed: `err`"
            downgraded_exception <- err
            self$error_encountered <- TRUE
          }
        )
      }
      "!DEBUG After attempting downgraded connection.. success?: `!self$error_encountered`"
      "!DEBUG !is.null(downgraded_exception) = `!is.null(downgraded_exception)`"
      if (!is.null(downgraded_exception)) {
        "!DEBUG adding single failure messages"
        "!DEBUG Exception encountered: `self$original_exception`"
        self$failure_messages.append("ERROR: Exception encountered: {self$original_exception}")
        self$failure_messages.append("ERROR: Unable to connect to RStudio Connect at {self$orig_server}")
      } else {
        "!DEBUG adding double failure messages"
        "!DEBUG Exception encountered: `self$original_exception`"
        self$failure_messages.append("ERROR: Exception encountered: {self$original_exception}")
        self$failure_messages.append("ERROR: After attempting downgrade, exception encountered: {self$downgraded_exception}")
        self$failure_messages.append("ERROR: Unable to connect to RStudio Connect at {self$orig_server} or {self$downgraded_server}")
      }
      "!DEBUG connection has failed.."
      "!DEBUG connection failed: `self$failure_messages`"
      FALSE
    },
    check_for_http_error = function(res) {
      if (httr::http_error(res)) {
        self$failure_messages.append(sprintf(
          "%s request failed with %s, full response=%s",
          res$request$url,
          httr::http_status(res)$message,
          capture.output(str(httr::content(res)))
        ))
        return(TRUE)
      }
      return(FALSE)
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
#' @param allow_downgrade Whether or not to allow HTTPS to be downgraded to HTTP
#'
#' @return An Client object
#'
#' @rdname connect
#'
#' @export
connect <- function(server = Sys.getenv("CONNECT_SERVER", NA_character_),
                    api_key = Sys.getenv("CONNECT_API_KEY", NA_character_),
                    allow_downgrade = Sys.getenv("PLUMBERTABLEAU_ALLOW_DOWNGRADE", "FALSE")
                   ) {
  "!DEBUG Creating Connect object, server=`server`, api_key=`api_key`, allow_downgrade=`allow_downgrade`"
  allow_downgrade <- allow_downgrade == "TRUE"            
  Client$new(server = server, api_key = api_key, allow_downgrade = allow_downgrade)
}
