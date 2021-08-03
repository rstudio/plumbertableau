test_that("mock_tableau_request() works", {
  json <- jsonlite::prettify(jsonlite::toJSON(list(
    script = jsonlite::unbox("/foo"),
    data = list(
      `_arg1` = letters,
      `_arg2` = 1:length(letters)
    )
  )))

  expect_identical(mock_tableau_request("/foo", list(x = letters, y = 1:length(letters))),
                   json)

  expect_error(mock_tableau_request("/foo", list(x = 1:3, y = 1:5)),
               "^All entries in data must be of equal length.$")

  expect_error(mock_tableau_request("/foo", "this is not a list"),
               "data must be a list or data.frame object")
})
