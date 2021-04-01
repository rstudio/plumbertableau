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
#* @tab.arg str_value:[character]? Strings to be converted to lowercase
#* @tab.return [character] A lowercase string
#* @post /lowercase
#* @parser json
function(req, res, unicode = FALSE) {
  tolower(str_value)
}

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
