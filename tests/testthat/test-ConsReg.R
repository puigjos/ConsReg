context("Tests output ConsReg")
test_that("tests for dimension", {
  data('fake_data')
  fit1 = ConsReg(formula = y~x1+x2+x3+ I(x3^2) + x4, family = 'gaussian',
                 optimizer = 'mcmc',
                 data = fake_data)
  expect_true(length(fit1$coefficients) == ncol(fit1$x))
  expect_true(
   nrow(fit1$x) == length(as.numeric(fit1$fitted))
  )
  testthat::expect_error(ConsReg(formula = y~x1+x2+x3+ I(x3^2) + x4, family = 'gaussian',
                                 optimizer = 'mcmc', LOWER = 0,
                                 data = fake_data))
  testthat::expect_error(ConsReg(formula = y~x1+x2+x3+ I(x3^2) + x4, family = 'gaussian',
                                 optimizer = 'solnp', UPPER = 0,
                                 data = fake_data))

  })
