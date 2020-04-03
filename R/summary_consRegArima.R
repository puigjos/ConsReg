
summary.ConsRegArima <- function(object, ...){

  if(object$optimizer.name =='MCMCmetrop.opt'){
    se = apply(object$optimizer, 2, stats::sd, na.rm = T)/object$n.used
  } else if(object$optimizer.name == 'mcmc.opt'){
    se = apply(object$optimizer$pars, 2, stats::sd, na.rm = T)/object$n.used
  }else if (object$optimizer.name == 'adaptMCMC.opt'){
    se = apply(object$optimizer$sample, 2, stats::sd, na.rm = T)/object$n.used
  }else{
    if(!is.null(object$hessian)){
      se = sqrt(diag(solve(object$hessian * object$n.used)))
    }else{
      se = rep(NaN, length(object$coefficients))
    }

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
              residuals = as.numeric(object$residuals),
              aic = object$aic,
              bic = object$bic,
              aicc = object$aicc)
  class(res) <- "summary.ConsRegArima"
  res
}

print.summary.ConsRegArima <- function(x){
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat('Residuals:\n')
  print(summary(as.numeric(x$residuals)))
  cat('\n')
  stats::printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=TRUE,
               digits = 5)
  print(x$metrics)
  cat("AIC=", format(round(x$aic, 2L)), sep = "")
  cat("   AICc=", format(round(x$aicc, 2L)), sep = "")
  cat("   BIC=", format(round(x$bic, 2L)), "\n", sep = "")



}
