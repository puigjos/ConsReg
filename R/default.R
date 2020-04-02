
#' @rdname ConsReg
#' @export
ConsReg.default <- function(x, y, family, optimizer, ini.pars.coef = NULL,
                            constraints = NULL,
                            LOWER = NULL, UPPER = NULL, penalty = 1000,
                            ...){

  fit <- estimation(x = x, y = y, family = family,
                    optimizer = optimizer,
                    constraints = constraints,
                    LOWER = LOWER, UPPER = UPPER, penalty = penalty,
                    ini.pars.coef = ini.pars.coef,
                    ...)
  fit$fitted = family()$fitted(fit$coefficients, x)
  fit$metrics = family()$error_fun(fitted = fit$fitted, y = y)
  fit$residuals = y - fit$fitted
  fit$call = match.call()
  class(fit) = 'ConsReg'

  return(fit)
}


