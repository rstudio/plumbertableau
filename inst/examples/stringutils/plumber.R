library(plumber)
library(plumbertableau)
library(zeallot)

#* @apiTitle A simple Tableau Extensions API

#* Capitalize incoming text
#* @post /capitalize
#* @parser json
tableau_handler(
  args = list(
    str_value = "character"
  ),
  function(req, res) {
    toupper(str_value)
  }
)

#* Lowercase incoming text
#* @post /lowercase
#* @parser json
tableau_handler(
  args = list(
    str_value = "character"
  ),
  function(req, res) {
    c(dat) %<-% validate_request(req,
                                 x = "character")
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
  function(req, res, sep = " ") {
    do.call(paste, c(unname(.data), list(sep = sep)))
  }
)

#* @plumber
tableau_extension("stringutils")
