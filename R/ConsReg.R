
#'
#' Fit a regression model with gaussian or binomial objective function
#'
#' ConsReg is a function that allows to estimate a regression model:
#' linear regression (gaussian), logistic regression (binomial) or poisson regression.  It allows
#' the introduction of restrictions (both lower and upper limits) and restrictions
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
#' @param family a description of the error distribution and link
#' function to be used in the model. Possible values are: "gaussian" (linear regression) or
#' "binomial" (logistic regression) and "poisson"
#' @param optimizer Optimizer package used for fit the model
#' (include bayesian and genetic algorithm optimization).
#'  Possible values are: "solnp" (default) (Rsolnp),
#' "gosonlp" (Rsolnp),  "optim" (stats::optim), "nloptr" (nloptr), DEoptim ("DEoptim"),
#' "dfoptim" (dfoptim), "mcmc" (FME::modMCMC), "MCMCmetrop" (MCMCpack::MCMCmetrop1R),'adaptMCMC'(adaptMCMC::MCMC),
#' "GA" (GA package), "GenSA" (GenSA package)
#' @param LOWER (default NULL) vector of lower bounds for the coefficients.
#' If the lenght of LOWER is not equal with the length of the coeefficients, then, the rest will be equal to -Inf
#' @param UPPER (default NULL) vector of lower bounds for the coefficients.
#' If the lenght of UPPER is not equal with the length of the coeefficients, then, the rest will be equal to +Inf
#' @param penalty (default 1000) penalty to the objective function if some constraints do not fullfill
#' @param constraints vector of constraints (see details)
#' @param ini.pars.coef vector of initial parameters. In case there is some constraint,
#' then the ini.pars.coef should fulfill the constraints
#' @param na.action na.action to the data
#' @param x matrix of predictive variables
#' @param y vector of outcome variable
#' @param ... additional parameters passed in the optimizer (number of iterations, ...)
#'
#' @return An object of class "\code{ConsReg}".
#' \item{coefficients}{Coefficients of the regression}
#' \item{hessian}{hessian matrix if the optimizer can return it}
#' \item{family}{Model family function}
#' \item{optimizer}{optimizer object return (see details of each optimization package)}
#' \item{optimizer.name}{name of the optimizer}
#' \item{df}{nrow(data) - number of coefficients}
#' \item{rank}{number of coefficients}
#' \item{residuals}{residuals of the model}
#' \item{fitted}{fitted values of the model}
#' \item{metrics}{Accuracy metrics of the model}
#' \item{call}{the matched call}
#' \item{y}{objective variable}
#' \item{x}{regressors}
#' \item{formula}{formula term}
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
#' @author Josep Puig Sallés
#' @examples
#' data('fake_data')
#' fit1 = ConsReg(formula = y~x1+x2+x3+ I(x3^2) + x4, family = 'gaussian',
#'                      optimizer = 'mcmc',
#'                      data = fake_data)
#' summary(fit1)
#'
#' # We impose constraints to x3 and x3^2 and x4
#' fit2 = ConsReg(formula = y~x1+x2+x3+ I(x3^2) + x4, data = fake_data,
#'             family = 'gaussian',
#'             constraints = '(x3 + `I(x3^2)`) > .01, x4 < .2',
#'             optimizer = 'mcmc',
#'             ini.pars.coef = c(-1.65, .12, -.004, 0.1, 0.1, .15))
#'
#' fit1$coefficients
#' fit2$coefficients
#'
#' @export

ConsReg <- function(...){
  UseMethod('ConsReg')
}
