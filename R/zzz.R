.onLoad <- function(libname, pkgname) {
  # Debugging
  debugme::debugme()

  # Options
  op <- options()
  op.plumbertableau <- list(
    plumbertableau.warnings = TRUE
  )

  toset <- !(names(op.plumbertableau) %in% names(op))
  if(any(toset)) options(op.plumbertableau[toset])

  invisible()
}
