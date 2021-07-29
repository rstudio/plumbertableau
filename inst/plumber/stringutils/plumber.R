library(plumber)
library(plumbertableau)

#* @apiTitle String utilities
#* @apiDescription Simple functions for mutating strings

#* Lowercase incoming text
#* @param unicode:boolean Whether unicode logic should be used
#* @tableauArg str_value:[character] Strings to be converted to lowercase
#* @tableauReturn [character] A lowercase string
#* @post /lowercase
function(str_value, unicode = FALSE) {
  tolower(str_value)
}

#* Concatenate
#* @post /concat
#* @param sep:str Separator value to use
#* @tableauArg arg1:[character] One or more string values
#* @tableauArg arg2:[character] One or more string values to concatenate to `arg1`
#* @tableauReturn [character] arg1 and arg2 concatenated together
function(arg1, arg2, sep = " ") {
  paste(arg1, arg2, sep = sep)
}

#* Convert to string
#* @post /stringify
#* @tableauArg value:[any] One or more values of any data type
#* @tableauReturn [character] The data, converted to string
function(value) {
  if (is.logical(value)) {
    ifelse(value, "true", "false")
  } else {
    as.character(value)
  }
}

#* @plumber
tableau_extension
