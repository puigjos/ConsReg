
#' Predict function for ConsRegArima object
#'
#' Obtains predictions of ConsRegArima object
#' @param object ConsRegArima object
#' @param h horizont to predict
#' @param newdata data frame in which to look for variables with which to predict.
#' In case there is no regression part, this parameter could be set NULL
#' @param intervals Confidence level for prediction intervals (default 90)
#' @param origdata Original data (default NULL). Useful if lagged predictive
#' variables are used in the formula
#' @origdata Your original data. You must used if you have introduced lags in the formula
#'
#' @return
#' Returns an object of class predict.ConsRegArima
#'
#' \item{predict}{dataframe with the predictions}
#' \item{table}{dataframe with the predictions as well as the fitted values}
#' \item{intervals}{Interval level}
#' \item{object}{original object}
#'
#' @export
#'
#' @examples
#' data('series)
#' # Con contraints:
#' fit1 = ConsRegArima(formula = y ~ x1+x2 +x3+x4, order = c(2, 1),
#' data = series[1:60,])
#' pred = predict(fit1, h = 3, newdata = series[61:63,])
#' pred
#' plot(pred)
#'
predict.ConsRegArima <- function(object,
                                 h = ifelse(is.null(newdata), 1, nrow(newdata)),
                                 newdata = NULL, intervals = 90, origdata = NULL){

  if(ncol(object$x) == 0){
    newdata = data.frame(rep(NA, h))
  }
  if(ncol(object$x) == 1){
    if(colnames(object$x)[1] == '(Intercept)'){
      newdata = data.frame(rep(NA, h))
    }
  }
  if(nrow(newdata) != h){
    h = nrow(newdata)
  }

  n = length(object$y)

  x = object$x
  coef = object$coefficients
  coef_reg = coef[0:ncol(x)]
  coef_arma = coef[(ncol(x)+1):length(coef)]

  if(is.null(origdata)){
    TT = delete.response(terms(object$formula))
    mf <- model.frame(formula=TT, data=newdata)
    NEWx <- model.matrix(attr(mf, "terms"), data=mf)
  }else{
    TT = stats::delete.response(terms(object$formula))
    mf <- stats::model.frame(formula=TT, data=rbind(origdata, newdata))
    NEWx <- tail(stats::model.matrix(attr(mf, "terms"), data=mf), h)
  }



  predict_arima <- stats::KalmanForecast(n.ahead = h, mod = object$model)
  predict = as.numeric(NEWx %*% coef_reg + predict_arima$pred)


  table = data.frame(
    y = c(object$y, rep(NA, length(predict))),
    Fitted = c(object$fitted,  rep(NA, length(predict))),
    Prediction = c(rep(NA, length(object$y)), predict)
  )

  qq <- qnorm(0.5 * (1 + intervals/100))
  table$Fitted_Low = table$Fitted -  qq * sqrt(object$sigma2)
  table$Prediction_Low[(n+1):nrow(table)] = table$Prediction[(n+1):nrow(table)] -
    qq * sqrt(predict_arima$var * object$sigma2)

  table$Fitted_High = table$Fitted +  qq * sqrt(object$sigma2)
  table$Prediction_High[(n+1):nrow(table)] = table$Prediction[(n+1):nrow(table)] +
    qq * sqrt(predict_arima$var * object$sigma2)

  res = list(predict = table[is.na(table$Fitted),
                             c('Prediction', 'Prediction_High',
                               'Prediction_Low')],
             table = table,
             intervals = intervals, object = object)

  class(res) <-'predict.ConsRegArima'
  return(res)
}


#' @rdname predict.ConsRegArima
#' @export
print.predict.ConsRegArima <- function(x){
  print(x$table[is.na(x$table$Fitted),
                c('Prediction', 'Prediction_High',
                  'Prediction_Low')],
        digits = 3)
}


#' Plot the predictions
#'
#' @param x object of class predict.ConsRegArima
#'
#' @return
#' @export
#' @rdname predict.ConsRegArima
#' @examples
plot.predict.ConsRegArima <- function(x){
  table = x$table
  table$XXX = 1:nrow(table)

  ggplot2::ggplot(table, ggplot2::aes(x = XXX)) +
    ggplot2::geom_line(ggplot2::aes(y = y, color = 'Real')) +
    ggplot2::geom_line(ggplot2::aes(y = Fitted, color = 'Fitted')) +
    ggplot2::geom_line(ggplot2::aes(y = Prediction, color = 'Prediction')) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Fitted_Low, ymax = Fitted_High,
                             fill = 'Fitted'),
                alpha = .2, show.legend = F) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Prediction_Low, ymax = Prediction_High,
                    fill = 'Prediction'),
                alpha = .2,  show.legend = F) +
    ggplot2::labs(x = '', y = '', color = '')
}

