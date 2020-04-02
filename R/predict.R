
#' Predict or fitted values of object \code{ConsReg}
#'
#' @param object object of class \code{ConsReg}
#' @param newdata New data to predict the objective function. If is NULL (default),
#' then the fitted values will be returned
#' @param components if its \code{TRUE}, it will return the predictions for each regression component
#' @param ... Additional argument passed to family. In particular, at this moment,
#' if type = 'link', then for binomial family, it will return the link values
#'
#' @return predictions
#' @export
#'
#' @examples
#' data('fake_data')
#' data = fake_data
#' data$y = 1/(1+exp(-data$y))
#' data$y = ifelse(data$y > .5, 1, 0)
#' table(data$y)
#'
#' fit5 = ConsReg(y~x1+x2+x3+x4, data = data,
#'                family = 'binomial', penalty = 10000,
#'                LOWER = -.5, UPPER = .2,
#'                optimizer = 'gosolnp')
#' pr = predict(fit5, newdata = data[1:3,], type = 'probability')
#' pr
#'
predict.ConsReg <- function(object, newdata = NULL, components = F, ...){
  fam = object$family()
  if(is.null(newdata)){
    predict = fam$fitted(coef = object$coefficients,
                         x = object$x, components = components, ...)
  }else{
    TT = delete.response(terms(object$formula))
    mf <- model.frame(formula=TT, data=newdata)
    x <- model.matrix(attr(mf, "terms"), data=mf)
    predict = fam$predict(coef = object$coefficients,
                          newxreg = x, components = components, ...)
  }
  return(predict)
}
