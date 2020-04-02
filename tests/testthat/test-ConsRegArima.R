context("Tests output ConsRegArima")
test_that("tests for dimension", {
  data('series')
  fit1 = ConsRegArima(formula = y~-1, order = c(1, 1),
                 data = series)
  expect_true(length(fit1$coefficients) == 2)
  expect_true(
    nrow(fit1$x) == length(as.numeric(fit1$fitted))
  )
  testthat::expect_error(ConsRegArima(formula = y~-1, order = c(1, 1),
                                      LOWER = 1,
                                      data = series))
  testthat::expect_error(ConsRegArima(formula = y~-1, order = c(1, 1),
                                      UPPER = 1,
                                      data = series))

})
