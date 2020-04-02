#' Fit regression model with Arma errors to univariate time series
#'
#' ConsRegArima is a function that allows to estimate a regression model
#' with errors following an ARMA process (p,q). It allows the introduction of restrictions (both lower and upper limits) and restrictions
#' between the coefficients (in the form, for example, of a>b).
#'
#'
#' Several optimizers of various R packages are implemented,
#' including methods typically used in Bayesian regressions like Markov Chain Monte Carlo simulation.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of
#' the model to be fitted
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame)
#'  containing the variables in the model. If not found in data, the variables are taken from environment(formula),
#' typically the environment from which lm is called.
#' @param optimizer Optimizer package used for fit the model (include bayesian
#' and genetic algorithm optimization).
#'  Possible values are: "solnp" (default) (Rsolnp),
#' "gosonlp" (Rsolnp),  "optim" (stats::optim), "nloptr" (nloptr), DEoptim ("DEoptim"),
#' "dfoptim" (dfoptim), "mcmc" (FME::modMCMC), "MCMCmetrop" (MCMCpack::MCMCmetrop1R),
#' 'adaptMCMC'(adaptMCMC::MCMC),
#' "GA" (GA package), "GenSA" (GenSA package)
#' @param order Arma component (p, q)
#' @param seasonal A specification of the seasonal part of the ARMA model (P,Q),
#' plus the period (which defaults to 1).
#' @param LOWER (default NULL) vector of lower bounds for the coefficients.
#' If the lenght of LOWER is not equal with the length of the coeefficients, then, the rest will be equal to -Inf
#' @param UPPER (default NULL) vector of lower bounds for the coefficients.
#' If the lenght of UPPER is not equal with the length of the coeefficients, then, the rest will be equal to +Inf
#' @param penalty (default 1000) penalty to the objective function if some constraints do not fullfill
#' @param constraints vector of constraints (see details)
#' @param ini.pars.coef vector of initial parameters. In case there is some constraint,
#' then the ini.pars.coef should fulfill the constraints. This vector is only for regression component.
#' @param na.action na.action to the data
#' @param x matrix of predictive variables
#' @param y vector of outcome variable
#' @param model_fit object of class \code{ConsRegArima} to update the Arma part
#' and fix the coefficient from a previous model
#' @param ... additional parameters passed in the optimizer (number of iterations, ...)
#'
#' @return An object of class "\code{ConsRegArima}".
#' \item{coefficients}{Coefficients (regression + arma errors)}
#' \item{hessian}{hessian matrix if the optimizer can return it}
#' \item{optimizer}{optimizer object return (see details of each optimization package)}
#' \item{optimizer.name}{name of the optimizer}
#' \item{df}{nrow(data) - number of coefficients}
#' \item{rank}{number of coefficients}
#' \item{objective_function}{objective_function used}
#' \item{model}{A list representing the Kalman Filter used in the fitting}
#' \item{sigma2}{	the MLE of the innovations variance}
#' \item{residuals}{residuals of the model}
#' \item{fitted}{fitted values of the model}
#' \item{fitted_regression}{fitted values only of the regression part}
#' \item{fitted_arima}{fitted values only of the arma part}
#' \item{metrics}{Accuracy metrics of the model (accuracy function of the forecast package)}
#' \item{call}{the matched call}
#' \item{y}{objective series}
#' \item{x}{regressors}
#' \item{formula}{formula term}
#' \item{aic}{ the AIC value (see details)}
#' \item{bic}{the BIC value}
#' \item{aicc}{the AICc value}
#'
#' @details
#'
#' Constraints will be a string:
#' For example, if x1 and x2 are two coefficient names, then a constraint could be:
#' "x1 > x2" or "x1+x2 > 2". For some constraints, one can write: "x1+x2>2, x1 > 1".
#' Each constraint will be separate by commas.
#'
#' Important: if there are some constraints that do not fulfill in a model without those constraints,
#' it is recommended to use \code{ini.pars.coef} parameter to set initial values that fulfill constraints.
#' See the example
#'
#' On the other hand, aic value is computed as auto.arima function computes the AIC when method == 'CSS':
#' \deqn{ AIC = n * log(sigma2) + npar * 2 }
#' Where \code{npa}r I set the number of coefficients.
#'
#'
#' @references
#' Peiris, M. & Perera, B. (1988), On prediction with fractionally
#' Hyndman RJ, Khandakar Y (2008). “Automatic time series forecasting: the forecast package for R.”
#'
#' @author Josep Puig Salles
#'
#' @examples
#' data('series')
#' fit1 = ConsRegArima(formula = y ~ x1+x2 +x3+x4,
#'                     order = c(2, 1), data = series)
#' summary(fit1)
#' fit2 = ConsRegArima(formula = y ~ x1+x2 +x3+x4, order = c(2, 1),
#'                     data = series, constraints = '(x3 +.1) > x1',
#'                     ini.pars.coef = c(.96, .2, -.8, .4), UPPER = 1, LOWER = -1)
#'
#' fit1$coefficients
#' fit2$coefficients
#'
#' @export



ConsRegArima <- function(...){
  UseMethod('ConsRegArima')
}
