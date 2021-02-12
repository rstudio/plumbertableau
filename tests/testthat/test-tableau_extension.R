pr_path <- system.file("examples/stringutils/plumber.R", package = "plumbertableau")

test_that("stringutils example works", {

  expect_identical(
    tableau_invoke(pr_path, "/stringutils/capitalize", "hello"),
    "HELLO"
  )

  expect_identical(
    tableau_invoke(pr_path, "/stringutils/concat", letters, LETTERS),
    paste0(letters, " ", LETTERS)
  )

  expect_identical(
    tableau_invoke(pr_path, "/stringutils/concat?sep=-", letters, LETTERS),
    paste0(letters, "-", LETTERS)
  )
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
