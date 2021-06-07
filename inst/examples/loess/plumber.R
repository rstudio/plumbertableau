library(plumber)
library(plumbertableau)

#* @apiTitle Loess Smoothing
#* @apiDescription Loess smoothing for Tableau

#* Fit a loess curve to the inputs and return the curve values
#* @param alpha Degree of smoothing
#* @tab.arg x:integer X values for fitting
#* @tab.arg y:numeric Y values for fitting
#* @tab.return numeric Fitted loess values
#* @post /predict
function(req, res, x, y, alpha = 0.75) {
  alpha <- as.numeric(alpha)
  l_out <- loess(y ~ x, span = alpha)
  predict(l_out, data.frame(x, y))
}

#* @plumber
tableau_extension()
