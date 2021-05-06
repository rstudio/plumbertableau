pr_path <- system.file("examples/stringutils/plumber.R", package = "plumbertableau")

test_that("stringutils example works", {

  expect_identical(
    tableau_invoke(pr_path, "/stringutils/lowercase", "HeLlO"),
    "hello"
  )

  expect_identical(
    tableau_invoke(pr_path, "/stringutils/concat", letters, LETTERS),
    paste0(letters, " ", LETTERS)
  )

  expect_identical(
    tableau_invoke(pr_path, "/stringutils/concat?sep=-", letters, LETTERS),
    paste0(letters, "-", LETTERS)
  )

  # 404
  expect_error(tableau_invoke(pr_path, "/stringutils/blah", .quiet = TRUE))
  # Too few args
  expect_error(tableau_invoke(pr_path, "/stringutils/concat", letters, .quiet = TRUE))
  # Too many args
  expect_error(tableau_invoke(pr_path, "/stringutils/concat", letters, letters, letters, .quiet = TRUE))
  # Incorrect data type
  expect_error(tableau_invoke(pr_path, "/stringutils/concat", letters, seq_along(letters), .quiet = TRUE))
})

test_that("OpenAPI specification works", {
  pr <- plumber::plumb(pr_path)
  spec <- pr$getApiSpec()
  for (path in spec$paths) {
    if (!is.null(path$post)) {
      expect_equal(path$post$requestBody$description, "Tableau Request")
    }
  }
})
