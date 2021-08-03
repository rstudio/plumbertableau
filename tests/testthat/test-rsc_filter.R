  req <- make_req(
    verb = "POST",
    path = "/loess",
    postBody = encode_payload(script = "concat", letters, .toJSON_args = NULL, raw = FALSE),
    HTTP_X_RS_CORRELATION_ID = 123456
  )

test_that("rsc_filter() correctly modifies requests", {
  rsc_filter(req)
  expect_equal(req$HTTP_X_CORRELATION_ID, 123456)
})
