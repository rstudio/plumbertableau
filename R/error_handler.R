# Gracefully handle Plumber errors so they are effectively communicated back to
# Tableau
# @param req Plumber \code{req} object
# @param res Plumber \code{res} object
# @param err An error object passed from Plumber
error_handler <- function(req, res, err) {
  res$status <- 500
  list(
    message = jsonlite::unbox("Server Error"),
    info = jsonlite::unbox(conditionMessage(err))
  )
}
