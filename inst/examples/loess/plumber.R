library(plumber)
library(plumbertableau)

#* @apiTitle Loess Smoothing
#* @apiDescription Loess smoothing for Tableau

#* Fit a loess curve to the inputs and return the curve values
#* @param alpha Degree of smoothing
#* @parser json
#* @serializer json
#* @post /predict
tableau_handler(
  args = list(
    x = arg_spec(type = "integer", desc = "X values for fitting"),
    y = arg_spec(type = "numeric", desc = "Y values for fitting")
  ),
  return = return_spec(type = "numeric", desc = "Fitted loess values"),
  func = function(req, res, alpha = 0.75) {
    alpha <- as.numeric(alpha)
    l_out <- loess(y ~ x, span = alpha)
    predict(l_out, data.frame(x, y))
  }
)

#* @plumber
tableau_extension("/loess")
