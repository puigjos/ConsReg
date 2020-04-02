params <-
list(EVAL = TRUE)

## ----chunk_options, include=FALSE----------------------------------------
knitr::opts_chunk$set(
  fig.width = 6.5,
  fig.height = 3,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)
if (capabilities("cairo")) {
  knitr::opts_chunk$set(
    dev.args = list(png = list(type = "cairo"))
  )
}

## ---- warning=FALSE, message=FALSE---------------------------------------
require(ConsReg)
require(ggplot2)
require(data.table)

## ------------------------------------------------------------------------
data("fake_data")

## ------------------------------------------------------------------------
fit1 = ConsReg(formula = y~x1+x2+x3+ I(x3^2) + x4, family = 'gaussian',
                     optimizer = 'solnp',
                     data = fake_data)
fit1$coefficients
coef(lm(y~x1+x2+x3+I(x3^2) + x4, data = fake_data))


## ------------------------------------------------------------------------
fit1$metrics

## ------------------------------------------------------------------------
forecast::gghistogram(fit1$residuals, add.normal = T, add.rug = T) + 
  theme_minimal()

## ------------------------------------------------------------------------
fit2 = ConsReg(formula = y~x1+x2+x3+ I(x3^2) + x4, data = fake_data,
            family = 'gaussian',
            constraints = '(x3 + `I(x3^2)`) > .01, x4 < .2',
            optimizer = 'mcmc',
            LOWER = -1, UPPER = 1,
            ini.pars.coef = c(-.4, .12, -.004, 0.1, 0.1, .15))

## ------------------------------------------------------------------------
rbind(coef(fit1), 
      coef(fit2))

## ------------------------------------------------------------------------
rbind(fit1$metrics, 
      fit2$metrics)

## ------------------------------------------------------------------------
pred = data.frame(
  fit1 = predict(fit1, newdata = fake_data[2:3,]), 
  fit2 = predict(fit2, newdata = fake_data[2:3,])
  )
pred

## ------------------------------------------------------------------------
pr = predict(fit2, components = T, newdata = fake_data[5,])
pr

## ------------------------------------------------------------------------
data('series')

## ------------------------------------------------------------------------
plot(series$y, t='l')

## ------------------------------------------------------------------------
head(series)

## ------------------------------------------------------------------------
fit_ts1 = ConsRegArima(y ~ -1, order = c(1, 1), data = series[1:60, ])
fit_ts1$coefficients
coef(arima(series$y[1:60], order = c(1, 0, 1), include.mean = F, method = 'CSS'))

## ---- warning=FALSE------------------------------------------------------
fit_ts2 = ConsRegArima(y ~ x1+x2+x3+x4, order = c(1, 1), data = series[1:60,])

## ---- warning=FALSE------------------------------------------------------
fit_ts3 = ConsRegArima(y ~ x1+x2+x3+x4, order = c(1, 1), data = series[1:60,], 
                       LOWER = -1, UPPER = 1, 
                       constraints = "x4 < x2", 
                       ini.pars.coef = c(.9, .3, -.1, .3, -.3), 
                       control = list(trace = 0) #  not show the trace of the optimizer 
                       )
fit_ts3$coefficients

## ------------------------------------------------------------------------
fit_ts4 = ConsRegArima(y ~ x1+x2+x3+x4, order = c(1, 1), data = series[1:60,],
                       LOWER = -1, UPPER = 1,
                       constraints = "x4 < x2",
                       penalty = 10000,
                       optimizer = 'GA', maxiter = 1000,
                       monitor = NULL, #  not show the trace of the optimizer
                       ini.pars.coef = c(.9, .2, 0, .3, -.6)
                       )
fit_ts4$coefficients

## ------------------------------------------------------------------------
data.frame(
  metrics = colnames(fit_ts1$metrics),
  fit_ts1 = as.numeric(fit_ts1$metrics), 
  fit_ts2 = as.numeric(fit_ts2$metrics), 
  fit_ts3 = as.numeric(fit_ts3$metrics),
  fit_ts4 = as.numeric(fit_ts4$metrics)
  )

## ------------------------------------------------------------------------
pred = predict(fit_ts4, newdata = series[61:63, ], h=3, intervals = 90)
pred$predict

## ---- warning=FALSE------------------------------------------------------
plot(pred) + theme_minimal()

## ---- warning=FALSE------------------------------------------------------
fit_ts5 = ConsRegArima(y ~ x1+x3+
                         shift(x3, 2) + # x2 from 2 periods above
                         x4, 
                       order = c(1, 1), data = series[1:60,], 
                       seasonal = list(order = c(1, 0), period = 4), # seasonal component
                       control = list(trace = 0)
                       )

## ------------------------------------------------------------------------
pred = predict(fit_ts5, newdata = series[61:63,], origdata = series[1:60,])
pred$predict

## ------------------------------------------------------------------------
ro = rolling(object = fit_ts3, used.sample = 50, 
             refit = 4, h = 4, orig.data = series)

## ------------------------------------------------------------------------
plot(ro) + theme_minimal()

## ---- digits = 3---------------------------------------------------------
ro$results

