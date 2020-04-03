

estimationArima <- function(y, x, order, optimizer, seasonal,
                            LOWER = NULL, UPPER = NULL, penalty = 1000,
                            constraints = NULL,
                            ini.pars.coef = NULL,
                            ...){

  n = length(y)
  arma <- as.integer(c(order, seasonal$order,
                       ifelse(is.na(seasonal$period), 1, seasonal$period),
                       0,0))
  total_params = ncol(x) + sum(arma[1:4])
  ar = seq_len(order[1]) ; ma = seq_len(order[2])
  sar = seq_len(seasonal$order[1]) ; sma = seq_len(seasonal$order[2])
  mal = length(ma); arl = length(ar)
  smal = length(sma); sarl = length(sar)

  max.order = arma[1] + arma[5] * arma[3]

  # Prepare bounds
  prepareBounds <- function(upper, lower, total_params){
    if(is.null(upper)) upper = rep(Inf, total_params)
    if(length(upper) == 1) upper = rep(upper, total_params)
    if(length(upper) != total_params)
      upper = c(upper, rep(Inf,total_params-length(upper)))
    if(is.null(lower))lower = rep(-Inf, total_params)
    if(length(lower) == 1)lower = rep(lower, total_params)
    if(length(lower) != total_params)
      lower = c(lower, rep(-Inf,total_params-length(lower)))
    lower = lower[1:total_params]
    upper = upper[1:total_params]
    return(list(upper = upper, lower = lower))
  }

  bounds = prepareBounds(upper = UPPER, lower = LOWER, total_params =  total_params)

  # Ini values

  coef_reg <- stats::lm.fit(y = y, x = x)
  coef_arma = stats::arima(stats::residuals(coef_reg), order = c(arma[1], 0, arma[2]),
            seasonal = list(order = c(arma[3], 0, arma[4]), period = arma[5]),
            include.mean = F)$coef

  coef_reg = coef_reg$coefficients
  i = 1:length(coef_reg)

  if(length(coef_reg)>0){
    coef_reg <- ifelse(data.table::between(coef_reg,
                                           upper = bounds$upper[i],
                                           lower = bounds$lower[i]), coef_reg,
                       stats::runif(1, min = bounds$lower[i], max = bounds$upper[i]))
  }

  if(!is.null(ini.pars.coef)){
    if(length(ini.pars.coef) != length(coef_reg)){
      warning('Length of ini.pars.coef is not equal to parameters in the formula.
              We use inital default parameter')
    }else{
      coef_reg = ini.pars.coef
    }
  }

  ini_vars = c(coef_reg, coef_arma)

  # optimization
  names(ini_vars) = c(colnames(x),
                  if(arl > 0){paste0('ar', ar)}else{NULL},
                  if(mal > 0){paste0('ma', ma)}else{NULL},
                  if(sarl > 0){paste0('sar', sar)}else{NULL},
                  if(smal > 0){paste0('sma', sma)}else{NULL})
  nms = names(ini_vars)

  fit = do.optimizer(optimizer, fun = arma_fun2,
                     arma = arma,
                     LOWER = bounds$lower, UPPER = bounds$upper,
                     penalty = penalty, constraints = constraints,
                     x = x, y = y, pars = ini_vars, nms = nms,
                     ...)

# Coefficients ------------------------------------------------------------
  coef = fit$coef
  names(coef) = nms

  list(coefficients = coef,
       arma = arma,
       hessian = fit$hessian,
       optimizer = fit$optimizer,
       optimizer.name = optimizer,
       df = n - length(coef),
       rank = length(coef)
  )

}
