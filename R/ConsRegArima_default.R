
#' @rdname ConsRegArima
#' @export
ConsRegArima.default <- function(x, y, order,
                                 seasonal, optimizer,
                                 LOWER = NULL, UPPER = NULL, penalty = 1000,
                                 constraints = NULL,
                                 ini.pars.coef, model_fit = NULL,
                                 ...){

  fit = list()
  if(is.null(model_fit)){
    fit <- estimationArima(x = x, y = y, order = order,
                           seasonal,
                           optimizer = optimizer,
                           LOWER = LOWER, UPPER = UPPER, penalty = penalty,
                           constraints = constraints,
                           ini.pars.coef = ini.pars.coef,
                           ...)

    coef = fit$coefficients
    arma <- fit$arma
  }else{
    coef = model_fit$coefficients
    fit$coefficients = coef
    order = model_fit$order
    arma <- model_fit$arma
    fit$arma = arma
  }



  coef_arma = coef[(ncol(x)+1):length(coef)]
  coef_reg = coef[0:ncol(x)]

  n = length(y)
  SSinit = "Gardner1980"
  kappa = 1e+06

  trarma <- ArimaTransf(coef_arma, arma)
  model <- stats::makeARIMA(trarma[[1L]], trarma[[2L]], numeric(0), kappa,
                     SSinit)

  ArimaMatrix(y - x %*% coef_reg, 0L, TRUE, model$phi,
              model$theta, model$Delta, model$a,
              model$P, model$Pn)

  max.order = arma[1] + arma[5] * arma[3]
  val <- ArimaEstimation(y - x %*% coef_reg,
                         arma, trarma[[1L]], trarma[[2L]],
                         max.order, T)
  sigma2 <- val[[1L]]

  npar = length(coef)

  fit$aic <- n * log(sigma2) + 2 * npar
  fit$bic <- fit$aic + npar * (log(n) - 2)
  fit$aicc <- fit$aic + 2 * npar * (npar + 1) /(n - npar - 1)
  fit$model = model
  fit$sigma2 = sigma2
  fit$n.used = n
  fit$order = order
  tmp = getFittedArima(object = fit, x = x, y = y)
  fit$fitted = tmp$fitted
  fit$metrics = forecast::accuracy(fit$fitted, y)
  fit$residuals = tmp$residuals
  fit$fitted_regression = tmp$fitted_regression
  fit$fitted_arima = tmp$fitted_arima

  fit$call = match.call()
  class(fit) = 'ConsRegArima'

  return(fit)
}
