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
function(req, res) {
  c(dat) %<-% validate_request(req,
                               x = "character")
  tolower(dat)
}

#* Concatenate
#* @post /concat
#* @parser json
function(req, res, sep = " ") {
  do.call(paste, c(unname(req$body$data), list(sep = sep)))
}

#* @plumber
tableau_extension("stringutils")
