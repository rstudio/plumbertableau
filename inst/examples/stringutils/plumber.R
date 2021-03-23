library(plumber)
library(plumbertableau)

#* @apiTitle String utilities
#* @apiDescription Simple functions for mutating strings

#* Capitalize incoming text
#* @post /capitalize
#* @parser json
tableau_handler(
  args = list(
    str_value = "character"
  ),
  return = "character",
  function(req, res) {
    toupper(str_value)
  }
)

#* Lowercase incoming text
#* @param unicode:boolean Whether unicode logic should be used
#* @post /lowercase
#* @parser json
tableau_handler(
  args = list(
    str_value = arg_spec("character?", "Strings to be converted to lowercase")
  ),
  return = return_spec("character", "A lowercase string"),
  function(req, res, unicode = FALSE) {
    tolower(str_value)
  }
)

#* Concatenate
#* @post /concat
#* @parser json
tableau_handler(
  args = list(
    arg1 = "character",
    arg2 = "character"
  ),
  return = "character",
  function(req, res, sep = " ") {
    paste(arg1, arg2, sep = sep)
  }
)

#* @plumber
tableau_extension("stringutils")
