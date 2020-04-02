
#'
#' @rdname ConsReg
#' @export

ConsReg.formula <- function(formula, data=list(),
                            optimizer = 'solnp',
                            family = c('gaussian', 'binomial'),
                            constraints = NULL,
                            LOWER = NULL, UPPER = NULL, penalty = 1000,
                            na.action = 'na.omit',
                            ini.pars.coef = NULL,
                            ...)
{


  if((is.null(LOWER) & !is.null(UPPER)) | is.null(UPPER) & !is.null(LOWER)){
    stop('If LOWER/UPER is not NULL, then UPPER/LOWER cannot be NULL')
  }

  family = switch(family,
                 'gaussian' = gaussian.objective,
                 'binomial' = binomial.objective,
                 'poisson' = poisson.objective)
  optimizer = switch(optimizer,
                     'solnp' = 'solnp.opt',
                     'gosolnp' = 'gosolnp.opt',
                     'optim' = 'optim.opt',
                     'nloptr' = 'nloptr.opt',
                     'DEoptim' = 'DEoptim.opt',
                     'dfoptim' = 'hjkb.opt',
                     'mcmc' = 'mcmc.opt',
                     'MCMCmetrop' = 'MCMCmetrop.opt',
                     'adaptMCMC' = 'adaptMCMC.opt',
                     'GenSA' = 'GenSA.opt',
                     'GA' = 'ga.opt')

  if (missing(data))
    data <- environment(formula)

  mf <- stats::model.frame(formula=formula, data=data, na.action = na.action)
  x <- stats::model.matrix(attr(mf, "terms"), data=mf)
  y <- stats::model.response(mf)
  est <- ConsReg.default(x = x, y = y,
                         optimizer = optimizer, family = family,
                         ini.pars.coef = ini.pars.coef,
                         constraints = constraints,
                         LOWER = LOWER, UPPER = UPPER, penalty = penalty,
                         ... )
  est$call <- match.call()
  est$formula <- formula
  est$x <- x
  est$y <- y
  est
}
