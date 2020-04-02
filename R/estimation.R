

estimation <- function(x, y, family = gaussian.objective, optimizer = 'solnp.opt',
                       ini.pars.coef = NULL,
                       constraints = NULL,
                       LOWER = NULL, UPPER = NULL, penalty = 1000,
                       ...){
  f = family()

  bounds = f$prepareBounds(upper = UPPER, lower = LOWER, x = x)

  if(length(bounds$upper) != ncol(x)){
    stop('The length of the upper or lower vector does
         not match the number of parameters in the model')
  }

  if(is.null(ini.pars.coef)){
    ini_vars = do.call(f$ini_vars,
                       args = list(x = x, y = y, upper = bounds$upper,
                                   lower = bounds$lower))
  }else{
    ini_vars = ini.pars.coef
    names(ini_vars) = colnames(x)
  }

  nms = names(ini_vars)
  fit = do.optimizer(optimizer, fun = f$objective,
                     LOWER = bounds$lower, UPPER = bounds$upper,
                     penalty = penalty, constraints = constraints,
                     x = x, y = y, pars = ini_vars, nms = nms,
                     ...)

  coef = f$final_coef(fit$coef)


  df = nrow(x) - ncol(x)
  names(coef) <- colnames(x)

  list(coefficients = coef,
       family = family,
       hessian = fit$hessian,
       optimizer = fit$optimizer,
       optimizer.name = optimizer,
       df = df,
       rank = length(coef)
       )
}
