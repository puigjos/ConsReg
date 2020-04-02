#' @rdname ConsRegArima
#' @export
ConsRegArima.formula <- function(formula, data=list(),
                                 optimizer = c('solnp'),
                                 order = c(0,0),
                                 seasonal = list(order = c(0,0), period = NA),
                                 LOWER = NULL, UPPER = NULL, penalty = 1000,
                                 constraints = NULL,
                                 ini.pars.coef = NULL,
                                 na.action = 'na.omit', ...){

  if((is.null(LOWER) & !is.null(UPPER)) | is.null(UPPER) & !is.null(LOWER)){
    stop('If LOWER/UPER is not NULL, then UPPER/LOWER cannot be NULL')
  }
  if(length(order) != 2){
    stop('Length of the order param must be equal to 2')
  }
  if(length(seasonal$order) != 2){
    stop('Length of the order param must be equal to 2')
  }

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
  y <- as.numeric(stats::model.response(mf))


  est <- ConsRegArima.default(x = x, y = y, order = order, seasonal = seasonal,
                              optimizer = optimizer,
                              LOWER = LOWER, UPPER = UPPER, penalty = penalty,
                              constraints = constraints,
                              ini.pars.coef = ini.pars.coef,
                              ...)
  est$call <- match.call()
  est$formula <- formula
  est$x <- x
  est$y <- y
  est

}
