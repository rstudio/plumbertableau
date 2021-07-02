test_that("OpenAPI specification works", {
  pr <- plumber::plumb(pr_path())
  spec <- pr$getApiSpec()
  for (path in spec$paths) {
    if (!is.null(path$post)) {
      expect_equal(path$post$requestBody$description, "Tableau Request")
    }
  }
})
