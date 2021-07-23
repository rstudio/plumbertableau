test_that("check_route() warnings fire as expected", {
  w <- capture_warnings(plumber::plumb(system.file("plumber/mounts/plumber.R", package = "plumbertableau")))
  expect_match(w[1], "^Tableau endpoints must accept POST requests. /bar does not respond to POST requests.$")
  expect_match(w[2], "^Route /bar includes a user specified parser. plumbertableau automatically sets the appropriate parser for Tableau requests. There is no need to specify a parser.$")
  expect_match(w[3], "^Route /bar includes a user specified serializer. plumbertableau automatically sets the appropriate serializer for Tableau requests. There is no need to specify a serializer.$")
})

test_that("write_log_message() includes required fields", {
  req <- as.environment(list(
    "HTTP_X_CORRELATION_ID" = "correlation_id_str",
    "REQUEST_METHOD" = "POST",
    "PATH_INFO" = "path_info_str",
    "postBody" = "postBody_str"
  ))
  msg <- "msg_str"
  log_msg <- write_log_message(req, NULL, msg)
  expect_true(all(stri_detect_fixed(log_msg, c(req$HTTP_X_CORRELATION_ID, req$REQUEST_METHOD, req$PATH_INFO, req$postBody, msg))))
})


test_that("write_log_message() does not include postBody if body_log field is present in request", {
  req <- as.environment(list(
    "HTTP_X_CORRELATION_ID" = "correlation_id_str",
    "REQUEST_METHOD" = "POST",
    "PATH_INFO" = "path_info_str",
    "body_log" = TRUE,
    "postBody" = "postBody_str"
  ))
  msg <- "msg_str"
  log_msg <- write_log_message(req, NULL, msg)
  expect_false(stri_detect_fixed(log_msg, req$postBody))
})
