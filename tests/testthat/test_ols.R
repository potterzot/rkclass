library(rkclass)
context("Test that OLS regression output is correct.")

#(Examples following Wooldridge 2002, pp.59, 61)
data(mroz)

#custom expect function to test model results.
expect_model_equal <- function(model_base, model_alt, tol=0.000001) {
  expect_equal(round(model_alt$coefficients[names(model_base$coefficients)],5),
               round(model_base$coefficients, 5))
  expect_less_than(model_alt$residuals - model_base$residuals, tol)
  expect_less_than(sum(model_alt$fitted.values - model_base$fitted.values), tol)
  expect_equal(model_alt$df.residual, model_base$df.residual)
  expect_equal(vcov(model_alt), vcov(model_base))
}

#######
# Tests
test_that("Model with instruments but set to OLS has correct values.", {
  formula = lwage ~ exper + expersq + educ | . - educ + age + kidslt6 + kidsge6
  m_ols = lm(lwage ~ exper + expersq + educ, data = mroz)
  m_k = kclass(formula, data = mroz, model.type = "OLS")
  expect_model_equal(m_k, m_ols)
})

test_that("Model with no instruments works.", {
  formula = lwage ~ exper + expersq + educ
  m_ols = lm(formula, data = mroz)
  m_k = kclass(formula, data = mroz)
  expect_model_equal(m_k, m_ols)
})

test_that("Model with only 1 regressor, no intercept, and no instruments works.", {
  formula = lwage ~ exper - 1
  m_ols = lm(formula, data = mroz)
  m_k = kclass(formula, data = mroz)
  expect_model_equal(m_k, m_ols)
})

test_that("Weighted model returns correct results.", {
  formula = lwage ~ exper + expersq + educ | . - educ + age + kidslt6 + kidsge6
  w = abs(rnorm(nrow(mroz)))
  w = w / sum(w)
  m_ols = lm(lwage ~ exper + expersq + educ, data = mroz, weights = w)
  m_k = kclass(formula, data = mroz, weights = w, model.type = "OLS")
  expect_model_equal(m_k, m_ols)
})

test_that("Returned OLS model has correct type and attributes.", {
  formula = lwage ~ exper + expersq + educ | . - educ + age + kidslt6 + kidsge6
  m_k = kclass(formula, data = mroz, model.type = "OLS")
  expect_equal(class(m_k),"kclass")
  expect_equal(m_k$model.type, "OLS")
  expect_equal(m_k$k, 0)
  expect_equal(m_k$rank, 4)
  expect_equal(m_k$n, 428)
  expect_equal(m_k$nobs, 428)
})

