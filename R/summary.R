

summary.ConsReg <- function(object, ...){
  family = object$family()
  VCOV <- family$vcov(coef = object$coefficients, x = object$x,
                      residuals = object$residuals,
                      fitted = object$fitted,
                      data = cbind(object$x, object$y))
  se <- sqrt(diag(VCOV))
  if(object$optimizer.name =='MCMCmetrop.opt'){
    se = apply(object$optimizer, 2, stats::sd, na.rm = T)/sqrt(object$rank)
  }
  if(object$optimizer.name == 'mcmc.opt'){
    se = apply(object$optimizer$pars, 2, stats::sd, na.rm = T)/sqrt(object$rank)
  }
  if(object$optimizer.name == 'adaptMCMC.opt'){
    se = apply(object$optimizer$sample, 2, stats::sd, na.rm = T)/sqrt(object$rank)
  }
  tval <- object$coefficients / se
  TAB <- cbind(Estimate = stats::coef(object),
               StdErr = se,
               t.value = tval,
               p.value = 2*stats::pt(-abs(tval), df = object$df))
  res <- list(call=object$call,
              coefficients=TAB,
              fitted = object$fitted,
              metrics = object$metrics,
              residuals = as.numeric(object$residuals))
  class(res) <- "summary.ConsReg"
  res
}

print.summary.ConsReg <- function(x){
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat('Residuals:\n')
  print(summary(as.numeric(x$residuals)))
  cat('\n')
  stats::printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=TRUE,
               digits = 5)
  print(x$metrics)
  cat("\n")


}
