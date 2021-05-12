test_that("check_connect() works", {
  expect_false(check_connect())
  Sys.setenv("R_CONFIG_ACTIVE" = "rsconnect")
  expect_true(check_connect())
  Sys.unsetenv("R_CONFIG_ACTIVE")
})

test_that("Tableau warnings fire as expected", {
  w <- capture_warnings(plumber::plumb(system.file("examples/mounts/plumber.R", package = "plumbertableau")))
  expect_match(w[1], "^Tableau endpoints must accept POST requests. /bar does not respond to POST requests.$")
  expect_match(w[2], "^Route /barincludes a user specified parser. plumbertableau automatically sets the appropriate parser for Tableau requests. There is no need to use #* @parser or pr_set_parsers().$")
})
