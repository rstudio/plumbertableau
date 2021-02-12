
# plumbertableau

<!-- badges: start -->
[![R-CMD-check](https://github.com/rstudio/plumbertableau/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/plumbertableau/actions)
<!-- badges: end -->

An R package that helps create [Plumber APIs](https://www.rplumber.io/) that are
compliant with the [Tableau Analytics Extension
API](https://tableau.github.io/analytics-extensions-api/#).

## Installation

You can install the development version of plumbertableau from
[GitHub](https://github.com/rstudio/plumbertableau) with:

``` r
remotes::install_github("rstudio/plumbertableau")
```

## Example

The main function in `plumbertableau` is `tableau_extension()`. This function creates a
function that takes an existing Plumber router and modifies it so that it
complies with the specification set forward by the Tableau Analytics Extensions
API.

``` r
library(plumber)
library(plumbertableau)

#* @apiTitle A simple Tableau Extensions API

#* Capitalize incoming text
#* @post /capitalize
function(req, res) {
  dat <- req$body$data
  toupper(dat)
}

#* @plumber
tableau_extension()
```

In order to use an analytics extension from Tableau, you need to configure the
extension for either [Tableau
Desktop](https://tableau.github.io/analytics-extensions-api/docs/ae_connect_desktop.html)
or [Tableau
Server](https://tableau.github.io/analytics-extensions-api/docs/ae_connect_server.html).

When this API is running, it can be used via a calculated field in Tableau. To
use to example above, you could create a calculated field like the following:

![Calculated field screen shot](vignettes/calculated-field.png)

A `SCRIPT_` function in Tableau takes two arguments: `string` and `expression`.
When using `plumbertableau`, the `string` argument is the path to the endpoint
you want to use (in this case `/capitalize`). The subsequent `expression` can be
a constant value (in this case `"Hello World"`) or a reference to a data object
in Tableau.
