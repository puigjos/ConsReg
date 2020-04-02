
getFittedArima <- function(object, x, y){

  arma = object$arma
  coef = object$coefficients

  n = length(y)

  coef_reg = coef[0:ncol(x)]
  coef_arma = coef[(ncol(x)+1):length(coef)]

  fitted_regression <- as.numeric(x %*% coef_reg)
  error_regression = as.numeric(y) - fitted_regression


  trarma <- ArimaTransf(coef_arma, arma)
  max.order = arma[1] + arma[5] * arma[3]
  res <- ArimaEstimation(y - x %*% coef_reg,
                         arma, trarma[[1L]], trarma[[2L]],
                         max.order, T)[[2]]
  residuals = res
  fitted_arima = error_regression - residuals

  list(
    fitted = fitted_arima + fitted_regression,
    residuals = residuals,  fitted_arima = fitted_arima,
    fitted_regression = fitted_regression, residuals_regression = error_regression
  )
}
