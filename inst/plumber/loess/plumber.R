library(plumber)
library(plumbertableau)

#* @apiTitle Loess Smoothing
#* @apiDescription Loess smoothing for Tableau

#* Fit a loess curve to the inputs and return the curve values
#* @param alpha Degree of smoothing
#* @tableauArg x:integer X values for fitting
#* @tableauArg y:numeric Y values for fitting
#* @tableauReturn numeric Fitted loess values
#* @post /predict
function(x, y, alpha = 0.75) {
  alpha <- as.numeric(alpha)
  l_out <- loess(y ~ x, span = alpha)
  predict(l_out, data.frame(x, y))
}

#* @plumber
tableau_extension
