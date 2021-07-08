library(plumber)
library(plumbertableau)

#* @plumber
function(pr) {
  pr %>%
    pr_post("/capitalize", tableau_handler(
      args = list(
        str_value = arg_spec(
          type = "character",
          desc = "String(s) to be capitalized"
        )
      ),
      return = return_spec(
        type = "character",
        desc = "Capitalized string(s)"
      ),
      func = function(str_value) {
        toupper(str_value)
      }
    )) %>%
    tableau_extension()
}
