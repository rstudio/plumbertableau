library(plumber)
library(plumbertableau)
library(zeallot)

#* @apiTitle A simple Tableau Extensions API

#* Capitalize incoming text
#* @post /capitalize
function(req, res) {
  c(dat) %<-% req$body$data
  toupper(dat)
}

#* Concatenate
#* @post /concat
function(req, res, sep = " ") {
  do.call(paste, c(unname(req$body$data), list(sep = sep)))
}

#* @plumber
tableau_extension("stringutils")
