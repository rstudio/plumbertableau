test_that("tableau_json works", {
  json <- jsonlite::toJSON(list(
    script = jsonlite::unbox("/foo"),
    data = list(
      `_arg1` = letters,
      `_arg2` = 1:length(letters)
    )
  ))

  expect_identical(tableau_json("/foo", list(x = letters, y = 1:length(letters))),
                   json)

  expect_error(tableau_json("/foo", list(x = 1:3, y = 1:5)),
               "^All entries in data must be of equal length.$")
})
