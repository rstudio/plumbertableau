test_that("evaluate requests are rewritten correctly", {
  evaluate_req <- make_req(
    verb = "POST",
    path = "/evaluate",
    postBody = encode_payload(script = "concat", letters, .toJSON_args = NULL, raw = FALSE)
  )
  reroute(evaluate_req)

  expect_equal(evaluate_req$PATH_INFO, "/concat")
})

test_that("info requests return the info endpoint", {
  info_req <- make_req(
    verb = "GET",
    path = "/info",
  )
  expect_type(reroute(info_req), "list")
  expect_equal(reroute(info_req)$description, "Plumber Tableau API")
})

test_that("requests with 'script': 'return int(1)' return 1L", {
  python_int1_req <- make_req(
    verb = "POST",
    path = "/evaluate",
    postBody = encode_payload(script = "return int(1)", .toJSON_args = NULL, raw = FALSE)
  )
  expect_equal(reroute(python_int1_req), 1L)
})
