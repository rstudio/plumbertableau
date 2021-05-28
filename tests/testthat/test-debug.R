pr_path <- system.file("examples/stringutils/plumber.R", package = "plumbertableau")

test_that("debug logging works", {
  withr::local_envvar(c("DEBUGME" = "plumbertableau"))
  devtools::reload()
  expect_output(tableau_invoke(pr_path, "/stringutils/capitalize", "hello"),
                "^(plumbertableau)")
})
