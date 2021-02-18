library(plumber)
library(plumbertableau)
library(zeallot)

#* @apiTitle A simple Tableau Extensions API

#* Capitalize incoming text
#* @post /capitalize
#* @parser json
function(req, res) {
  c(dat) %<-% req$body$data
  toupper(dat)
}

#* Lowercase incoming text
#* @post /lowercase
#* @parser json
function(req, res) {
  c(dat) %<-% req$body$data
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
