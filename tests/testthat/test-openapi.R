test_that("OpenAPI specification works", {
  pr <- plumber::plumb(pr_path())
  spec <- pr$getApiSpec()
  for (path in spec$paths) {
    if (!is.null(path$post)) {
      expect_match(path$post$requestBody$description, "^<h3>Tableau Request</h3>\n\n<p>This is a mock Tableau request.")
    }
  }
})
