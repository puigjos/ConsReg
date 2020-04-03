

#' rolling: Back-test your model
#'
#' Function for creating rolling density forecast from \code{ConsRegArima} models with
#' option for refitting every n periods.
#'
#' @param object ConsRegArima object
#' @param used.sample The starting point in the dataset from which
#' to initialize the rolling forecast.
#' @param refit  Determines every how many periods the model is re-estimated.
#' If \code{refit}=0, then no refit is doing
#' @param h The number of periods to forecast
#' @param orig.data data original which was used to estimate the ConsRegArima \code{object}
#' @param ... Additional params for predict function
#'
#' @return
#'
#' \item{results}{data.frame with Real, Prediction, Prediction_High,
#'  Prediction_Low and fitted values of the \code{object}}
#'  \item{refitT}{how many periods the model is re-estimated}
#'  \item{metrics}{Main metrics of the predictions}
#' @export
#'
#' @seealso  \code{\link[ConsReg]{plot.roll.ConsRegArima}}
#'
#' @examples
#' data('series')
#' fit1 = ConsRegArima(formula = y ~ x1+x2 +x3, order = c(2, 1),
#'                     optimizer = 'solnp', data = series)
#' roll = rolling(fit1, used.sample = 40,
#'                refit = 5, orig.data = series, h=3)
#' roll
#' plot(roll)
#'

rolling <- function(object, used.sample, refit, h = 1, orig.data,...){

  if(class(object) != "ConsRegArima"){
    stop('object class must be ConsRegArima')
  }

  n = object$n.used
  times = (1 + used.sample):(n-h)
  if(refit > 0){
    refitT = seq(min(times), max(times), by = refit)
  }else{
    refitT = 0
  }


  results = list()

  for(i in times){
    # Refit?
    if(i %in% refitT){
      newModel = eval(rlang::call_modify(object$call, data = orig.data[1:i,]))
    }else{
      newModel = eval(rlang::call_modify(object$call, model_fit = object,
                                  data = orig.data[1:i,]))
    }

    Predictions = stats::predict(newModel, h = h, newdata = orig.data[(i+1):(i+h), ], ...)
    Predictions = data.table::last(Predictions$predict)
    final = data.frame(xx = i+h, Real = object$y[i + h],
                       Predictions, Fitted = object$fitted[i+h])
    results[[i]] = final
  }

  results = data.table::rbindlist(results)
  results = list(results = results, refitT = refitT,
                 metrics = forecast::accuracy(results$Real, results$Prediction))
  class(results) = 'roll.ConsRegArima'
  return(results)
}



#' @export
print.roll.ConsRegArima <- function(x,...){
  print(x$metrics)
}

#' Plot an roll object
#' plot an roll.ConsRegArima object
#'
#' @param x roll.ConsRegArima object
#' @param ... Additional params passed to ggplot2::labs function
#'
#' @return
#' @export
#' @examples
plot.roll.ConsRegArima <- function(x,...){
  res = x$results
  ggplot2::ggplot(res, ggplot2::aes_(x = ~xx)) +
    ggplot2::geom_line(ggplot2::aes_(y = ~Real, color = 'Real')) +
    ggplot2::geom_line(ggplot2::aes_(y = ~Prediction, color  = 'Prediction'))+
    ggplot2::geom_ribbon(ggplot2::aes_(ymax = ~Prediction_High,
                    ymin = ~Prediction_Low, fill = 'Prediction' ),
                alpha = .2, show.legend = F) +
    ggplot2::labs(x = '', y='', color = '', ...)
}
