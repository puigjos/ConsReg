

gaussian.objective = function(theta, x, y, lower,
                              components, constraints = constraints,
                              upper, penalty, residuals){

  ini_vars <- function(x, y, upper, lower){
    coef_reg = stats::coef(stats::glm.fit(x = x, y = y, family = stats::gaussian()))
    coef_reg <- ifelse(data.table::between(coef_reg,
                                           upper = upper,
                                           lower = lower), coef_reg,
                       stats::runif(1, min = lower, max = upper))
    names(coef_reg) = c(colnames(x))
    return(coef_reg)
  }

  prepareBounds <- function(upper, lower, x){
    total_params = ncol(x)
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

  objective = function(theta, x, y, LOWER, UPPER, penalty, nms, constraints = NULL){

    names(theta) = nms
    if(is.null(constraints)){
      z = 0
    }else{
      text_split = strsplit(constraints, ',')[[1]]
      TH <- data.table::setDT(as.list(theta))
      z = sapply(text_split, function(cond){
        with(TH, !eval(rlang::parse_expr(cond)))
      })
      z = any(z) * 1
    }

    k           <- ncol(x) # get the number of columns (independent vars)
    beta        <- theta[1:k] # vector of weights intialized with starting values
    expected_y  <- x %*% beta  # X is dimension (n x k) and beta is dimension (k x 1)
    N = nrow(x)
    LL          <- 1/(2*N) * sum((y - expected_y)^2) +
      any(beta > UPPER[1:k]) * penalty +
      any(beta < LOWER[1:k]) * penalty + z * penalty
    return(LL)
  }

  final_coef <- function(coef){
    return(coef)
  }

  fitted = function(coef, x, components = F){
    expected_y  <- x %*% coef
    if(components){
      MatCoef = diag(coef)
      colnames(MatCoef) = names(coef)
      com = x %*% MatCoef
      expected_y = cbind(expected_y, com)
      colnames(expected_y)[1] = 'Total'
    }
    return(expected_y)
  }
  predict = function(coef, newxreg, components = F){

    expected_y  <- newxreg %*% coef
    if(components){
      MatCoef = diag(coef)
      colnames(MatCoef) = names(coef)
      com = newxreg %*% MatCoef
      expected_y = cbind(expected_y, com)
      colnames(expected_y)[1] = 'Total'
    }
    return(expected_y)
  }


  vcov = function(coef, x, residuals, data, fitted){
    solve(t(x) %*% x) * sum(residuals ^ 2) / (nrow(data) - ncol(data) + 1 - 1)
  }

  error_fun <- function(y, fitted){
    data.frame(
      RMSE = Metrics::rmse(y, fitted),
      MAE = Metrics::mae(y, fitted),
      MAPE = Metrics::mape(y, fitted),
      MSE = Metrics::mse(y, fitted),
      SMAPE = Metrics::smape(y,fitted)
    )
  }

  return(list(objective = objective,
              ini_vars = ini_vars,
              prepareBounds = prepareBounds,
              final_coef = final_coef,
              fitted = fitted,
              predict = predict,
              error_fun = error_fun,
              vcov = vcov))
}
