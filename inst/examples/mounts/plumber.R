library(plumber)
library(plumbertableau)

pr_bar <- pr() %>%
  pr_get("/bar", function() "bar", parser = "json")

#* @plumber
function(pr) {
  pr %>%
    pr_mount("/foo", pr_bar)
}

#* @plumber
tableau_extension
