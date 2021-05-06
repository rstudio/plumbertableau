library(plumber)
library(plumbertableau)

#* @apiTitle String utilities
#* @apiDescription Simple functions for mutating strings

#* Lowercase incoming text
#* @param unicode:boolean Whether unicode logic should be used
#* @tab.arg str_value:[character] Strings to be converted to lowercase
#* @tab.return [character] A lowercase string
#* @post /lowercase
#* @parser json
function(req, res, unicode = FALSE) {
  tolower(str_value)
}

#* Concatenate
#* @post /concat
#* @param sep:str Separator value to use
#* @tab.arg arg1:[character] One or more string values
#* @tab.arg arg2:[character] One or more string values to concatenate to arg1
#* @tab.return [character] arg1 and arg2 concatenated together
#* @parser json
function(req, res, sep = " ") {
  paste(arg1, arg2, sep = sep)
}

#* @plumber
tableau_extension("stringutils")
