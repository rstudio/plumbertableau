test_that("stringutils example works", {

  pr_path <- system.file("examples/stringutils/plumber.R", package = "plumbertableau")

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
