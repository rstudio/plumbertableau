
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plumbertableau

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/plumbertableau/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/plumbertableau/actions)
[![Codecov test
coverage](https://codecov.io/gh/rstudio/plumbertableau/branch/main/graph/badge.svg)](https://codecov.io/gh/rstudio/plumbertableau?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/plumbertableau)](https://CRAN.R-project.org/package=plumbertableau)
<!-- badges: end -->

plumbertableau lets you call external R code from Tableau workbooks via
[Tableau Analytics
Extensions](https://tableau.github.io/analytics-extensions-api/). You
achieve this by writing a plumbertableau extension, which is a
[Plumber](https://www.rplumber.io/) API with some extra annotations —
comments prefixed with `#*`.

``` r
library(plumber)
library(plumbertableau)

#* @apiTitle String utilities
#* @apiDescription Simple functions for mutating strings

#* Capitalize incoming text
#* @tableauArg str_value:[character] Strings to be capitalized
#* @tableauReturn [character] A capitalized string(s)
#* @post /capitalize
function(str_value) {
  toupper(str_value)
}

# The Plumber router modifier tableau_extension is required
#* @plumber
tableau_extension
```

plumbertableau extensions are used in Tableau’s *calculated fields*.
Let’s imagine we’ve published our extension to RStudio Connect and have
given it the custom URL `stringutils`. To use our `capitalize`
extension, we’d type the following into a Tableau calculated field, or
just copy and paste it from the automatically generated code samples.
(In real usage, you’ll probably replace `"Hello World"` with references
to Tableau data.)

    SCRIPT_STR("/stringutils/capitalize", "Hello World")

Before you use the extension in Tableau, Tableau needs to be able to
access it. plumbertableau integrates seamlessly with [RStudio
Connect](https://www.rstudio.com/products/connect/), a commercial
publishing platform that enables R developers to easily publish a
variety of R content types. Connect lets you host multiple extensions by
ensuring that requests from Tableau are passed to the correct extension.
It’s also possible to host plumbertableau extensions on your own
servers.

## Installation

You can install plumbertableau from CRAN or install the latest
development version from GitHub.

``` r
# From CRAN
install.packages("plumbertableau")

# From GitHub
remotes::install_github("rstudio/plumbertableau")

library(plumbertableau)
```

## Further Reading

You can read more about plumbertableau at
<https://rstudio.github.io/plumbertableau/>. There, you’ll find more
detail about writing plumbertableau extensions, publishing them to
RStudio Connect, configuring Tableau, and using your extensions in
Tableau.
