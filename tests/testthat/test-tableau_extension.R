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

  expect_identical(
    tableau_invoke(pr_path, "/stringutils/stringify", 1:10),
    as.character(1:10)
  )

  expect_identical(
    tableau_invoke(pr_path, "/stringutils/stringify", c(TRUE, FALSE, NA, TRUE)),
    c("true", "false", NA, "true")
  )

  # 404
  expect_error(tableau_invoke(pr_path, "/stringutils/blah", .quiet = TRUE))
  # Too few args
  expect_error(tableau_invoke(pr_path, "/stringutils/concat", letters, .quiet = TRUE))
  expect_error(tableau_invoke(pr_path, "/stringutils/stringify", .quiet = TRUE))
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

test_that("tableau_handler warns on missing function params", {
  args <- list(foo = arg_spec("character"), bar = arg_spec("character"))

  expect_warning(tableau_handler(args = args, return = return_spec(), func = function() {}))
  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(foo) {}))
  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(bar) {}))
  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(foo, bar) {}), NA)
  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(...) {}), NA)

  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(req, res) {}))
  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(req, res, foo) {}))
  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(req, res, bar) {}))
  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(req, res, foo, bar) {}), NA)
  expect_warning(tableau_handler(args = args, return = return_spec(), func = function(req, res, ...) {}), NA)
})
