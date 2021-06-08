test_that("Tableau warnings fire as expected", {
  w <- capture_warnings(plumber::plumb(system.file("examples/mounts/plumber.R", package = "plumbertableau")))
  expect_match(w[1], "^Tableau endpoints must accept POST requests. /bar does not respond to POST requests.$")
  expect_match(w[2], "^Route /bar includes a user specified parser. plumbertableau automatically sets the appropriate parser for Tableau requests. There is no need to specify a parser.$")
  expect_match(w[3], "^Route /bar includes a user specified serializer. plumbertableau automatically sets the appropriate serializer for Tableau requests. There is no need to specify a serializer.$")
})
