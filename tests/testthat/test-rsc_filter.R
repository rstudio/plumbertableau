  req <- make_req(
    verb = "POST",
    path = "/loess",
    postBody = encode_payload(script = "concat", letters, .toJSON_args = NULL, raw = FALSE)
  )

test_that("rsc_filter() correctly rewrites requests", {
  # TODO: Add test cases once rsc_filter() is finalized.
})