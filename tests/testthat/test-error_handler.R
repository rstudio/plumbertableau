test_that("error_handler returns correct object", {
  req <- as.environment(list())
  res <- as.environment(list())
  err <- rlang::catch_cnd(stop("This is an error"))
  handled_err <- error_handler(req = req, res = res, err = err)
  expect_equal(handled_err, list(
    message = jsonlite::unbox("Server Error"),
    info = jsonlite::unbox("This is an error")
  ))
  expect_equal(res$status, 500)
})
