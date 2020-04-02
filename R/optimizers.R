

# General -----------------------------------------------------------------


do.optimizer <- function(optimizer, fun, ...){
  do.call(optimizer, args = list(fun = fun, ...))
}


# gosolnp -----------------------------------------------------------------

gosolnp.opt <- function(pars, x, y, fun, UPPER, LOWER, ...){
  if(any(c(Inf, -Inf ) %in% c(LOWER, UPPER))){
    stop('Set real values to LOWER or UPPER params')
  }
  fit = Rsolnp::gosolnp(pars = pars, fun = fun, x = x, y = y,
          UB = UPPER, LB = LOWER, UPPER = UPPER,
          LOWER = LOWER, ...)

  return(
    list(
      coef = fit$pars, hessian = fit$hessian, values = fit$values,
      optimizer = fit
    )
  )
}


# solnp -------------------------------------------------------------------


solnp.opt <- function(pars, x, y, fun, ...){

  fit = Rsolnp::solnp(pars = pars, fun = fun, x = x, y = y,
              ...)

  return(
    list(
      coef = fit$pars, hessian = fit$hessian, values = fit$values,
      optimizer = fit
    )
  )

}

# optim -------------------------------------------------------------------


optim.opt <- function(pars, x, y, fun, method = 'L-BFGS-B',
                     UPPER, LOWER,
                      hessian = T,...){
  if(any(c(Inf, -Inf ) %in% c(LOWER, UPPER)) & method == 'L-BFGS-B'){
    stop('Set real values to LOWER or UPPER params for method = "L-BFGS-B"')
  }
  fit = stats::optim(par = pars, fn = fun, x = x, y = y,
              upper = UPPER,
              lower = LOWER,
              UPPER = UPPER,
              method = method,
              LOWER = LOWER,
              ...)

  return(
    list(
      coef = fit$par, hessian = fit$hessian, values = fit$values,
      optimizer = fit
    )
  )

}



# nloptr -----------------------------------------------------------------


nloptr.opt <- function(pars, x, y, fun,
                       opts = list("algorithm"="NLOPT_LN_COBYLA",
                              "xtol_rel"=1.0e-8),
                       ...){

  fit = nloptr::nloptr(x0 = pars, eval_f = fun,
               x = x, y = y, opts = opts,
              ...)
  return(
    list(
      coef = fit$solution, hessian = NULL, values = fit$objective,
      optimizer = fit
    )
  )

}


# DEoptim -----------------------------------------------------------------

DEoptim.opt <- function(pars, x, y, fun, LOWER, UPPER,
                       ...){
  if(any(c(Inf, -Inf ) %in% c(LOWER, UPPER))){
    stop('Set real values to LOWER or UPPER params')
  }
  fit = DEoptim::DEoptim(fn = fun,
               x = x, y = y,
               lower = LOWER, upper = UPPER,
               UPPER = UPPER,
               LOWER = LOWER,
               ...)
  return(
    list(
      coef = fit$optim$bestmem, hessian = NULL, values = fit$optim$bestval,
      optimizer = fit
    )
  )

}


# dfoptim -----------------------------------------------------------------

hjkb.opt <- function(pars, x, y, fun,
                      LOWER, UPPER,
                      ...){
  if(any(c(Inf, -Inf ) %in% c(LOWER, UPPER))){
    stop('Set real values to LOWER or UPPER params')
  }
  fit = dfoptim::hjkb(par = pars, fn = fun, x = x, y = y,
             upper = UPPER, UPPER = UPPER,
             lower = LOWER, LOWER = LOWER,
              ...)

  return(
    list(
      coef = fit$par, hessian = fit$hessian, values = fit$value,
      optimizer = fit
    )
  )

}




# Bayes -------------------------------------------------------------------



mcmc.opt <- function(pars, x, y, fun, ...){

  fit <- FME::modMCMC(f = fun, p = pars,
                      x = x, y = y, ... )

  return(
    list(
      coef = fit$bestpar,
      hessian = NULL,
      values = fit$bestfunp,
      optimizer = fit
    )
  )

}


MCMCmetrop.opt <- function(pars, x, y, fun,
                      thin=1, mcmc=40000, burnin=500,
                      ...){

  fit = MCMCpack::MCMCmetrop1R(fun = function(...){-fun(...)},
    theta.init = pars, x = x, y = y,
    thin=thin, mcmc=mcmc, burnin=burnin,
    ... )

  return(
    list(
      coef = colMeans(fit),
      hessian = NULL,
      values = NULL,
      optimizer = fit
    )
  )

}




# adaptMCMC ---------------------------------------------------------------

adaptMCMC.opt <- function(pars, x, y, fun,
                           samples = 5e4,
                           ...){

  fit = adaptMCMC::MCMC(p = function(...){-fun(...)},
                               init = pars, x = x, y = y,
                        n = samples,
                        ... )

  return(
    list(
      coef = colMeans(fit$samples),
      hessian = NULL,
      values = NULL,
      optimizer = fit
    )
  )

}


# Generalized Simulated Annealing Function ---------------------------------------------------------------

GenSA.opt <- function(pars, x, y, fun,
                      LOWER, UPPER, global.min,
                      ...){
  tol = 1e-13
  # global.min = 3.8
  if(!'global.min' %in% ls()){
    stop('You must set global.min = X in the function
         search for some value!')
  }

  fit = GenSA::GenSA(fn = fun,
                     x = x, y = y,
                     upper = UPPER, UPPER = UPPER,
                     lower = LOWER, LOWER = LOWER,
                     ... )
  return(
    list(
      coef = fit$par,
      hessian = NULL,
      values = NULL,
      optimizer = fit
    )
  )

}

# Genetic Algorithm -------------------------------------------------------



ga.opt <- function(pars, x, y, fun,
                   type = "real-valued",
                   popSize = 50, maxiter = 100,
                   LOWER, UPPER, control,
                   ...){
  sug = matrix(pars, nrow = 1)
  sub = rbind(sug, sug, sug)

  if(any(c(Inf, -Inf ) %in% c(LOWER, UPPER))){
    stop('Set real values to LOWER or UPPER params')
  }

  fit = GA::ga(fitness = function(...){-fun(...)},
               x = x, y = y, type = type,
               suggestions = sug,
               upper = UPPER, UPPER = UPPER,
               lower = LOWER, LOWER = LOWER,
               popSize = popSize, maxiter = maxiter,
                               ... )

  coef = as.numeric(fit@solution)
  names(coef) = names(pars)

  return(
    list(
      coef = as.numeric(fit@solution),
      hessian = NULL,
      values = NULL,
      optimizer = fit
    )
  )

}




