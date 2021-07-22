#
# This is a Tableau Extension built using plumbertableau.
#
# Find out more about building Tableau Extensions with plumbertableau here:
#
#    https://rstudio.github.io/plumbertableau/
#

library(plumber)
library(plumbertableau)

#* @apiTitle String utilities
#* @apiDescription Simple functions for mutating strings

#* Capitalize incoming text
#* @tab.arg str_value:[character] Strings to be capitalized
#* @tab.return [character] A capitalized string(s)
#* @post /capitalize
function(str_value) {
  toupper(str_value)
}

#* @plumber
tableau_extension
