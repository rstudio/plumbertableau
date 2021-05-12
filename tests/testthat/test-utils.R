test_that("Tableau warnings fire as expected", {
  w <- capture_warnings(plumber::plumb(system.file("examples/mounts/plumber.R", package = "plumbertableau")))
  expect_match(w[1], "^Tableau endpoints must accept POST requests. /bar does not respond to POST requests.$")
  expect_match(w[2], "^Tableau submits JSON requests. /bar does not include a JSON parser.$")
})
