test_that("stringutils example works", {

  expect_identical(
    tableau_invoke(pr_path(), "/lowercase", "HELLO"),
    "hello"
  )

  expect_identical(
    tableau_invoke(pr_path(), "/concat", letters, LETTERS),
    paste0(letters, " ", LETTERS)
  )

  expect_identical(
    tableau_invoke(pr_path(), "/concat?sep=-", letters, LETTERS),
    paste0(letters, "-", LETTERS)
  )

  expect_identical(
    tableau_invoke(pr_path(), "/stringify", 1:10),
    as.character(1:10)
  )

  expect_identical(
    tableau_invoke(pr_path(), "/stringify", c(TRUE, FALSE, NA, TRUE)),
    c("true", "false", NA, "true")
  )

  # 404
  expect_error(tableau_invoke(pr_path(), "/blah", .quiet = TRUE))
  # Too few args
  expect_error(tableau_invoke(pr_path(), "/concat", letters, .quiet = TRUE))
  expect_error(tableau_invoke(pr_path(), "/stringify", .quiet = TRUE))
  # Too many args
  expect_error(tableau_invoke(pr_path(), "/concat", letters, letters, letters, .quiet = TRUE))
  # Incorrect data type
  expect_error(tableau_invoke(pr_path(), "/concat", letters, seq_along(letters), .quiet = TRUE))
})
