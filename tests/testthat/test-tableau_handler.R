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
