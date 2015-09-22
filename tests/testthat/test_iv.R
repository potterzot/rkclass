library("rkclass")
context("Test IV-specific functions.")

#custom expect function to test model results.
expect_model_equal <- function(model_base, model_alt, tol=0.000001) {
  expect_equal(round(model_alt$coefficients[names(model_base$coefficients)],5),
               round(model_base$coefficients, 5))
  expect_less_than(model_alt$residuals - model_base$residuals, tol)
  expect_less_than(sum(model_alt$fitted.values - model_base$fitted.values), tol)
  expect_equal(model_alt$df.residual, model_base$df.residual)
  expect_equal(vcov(model_alt), vcov(model_base))
  expect_equal(model_alt$sigma, model_base$sigma)
  expect_equal(model_alt$cov.unscaled, model_base$cov.unscaled)
}

data(mroz)
formula = lwage ~ exper + expersq + educ | . - educ + age + kidslt6 + kidsge6
w = abs(rnorm(753))

#LIML
test_that("unweighted LIML matches TSLS.", {
  m = kclass(formula, data=mroz)
  m_liml = kclass(formula, data=mroz, model.type="LIML")
  skip("No outcome to test against yet.")
})
test_that("weighted LIML is correct.", {
  m = kclass(formula, data=mroz, weights=w)
  m_liml = kclass(formula, data=mroz, weights=w, model.type="LIML")
  skip("No outcome to test against yet.")
})

#FULLER
test_that("unweighted FULLER matches output.", {
  m = kclass(formula, data=mroz)
  m_fuller = kclass(formula, data=mroz, model.type="FULLER", alpha=2)
  skip("No data to test against yet.")
})
test_that("weighted FULLER is correct.", {
  m = kclass(formula, data=mroz, weights=w)
  m_fuller = kclass(formula, data=mroz, weights=w, model.type="FULLER", alpha=2)
  skip("No data to test against yet.")
})

#KCLASS
test_that("unweighted LIML matches TSLS.", {
  m = kclass(formula, data=mroz)
  m_liml = kclass(formula, data=mroz, model.type="KCLASS", k=2)
  #expect_less_than(mean(abs(coef(m_liml) - coef(m))), 0.00001)
  skip("no data to test kclass against yet.")
})
test_that("weighted LIML is correct.", {
  m = kclass(formula, data=mroz, weights=w)
  m_liml = kclass(formula, data=mroz, weights=w, model.type="KCLASS", k=2)
  #expect_less_than(mean(abs(coef(m_liml) - coef(m))), 0.00001)
  skip("No outcomes to test kclass against yet.")
})
test_that("setting k overrides other options", {
  m = kclass(formula, data=mroz, k=3)
  m2 = kclass(formula, data=mroz, k=3, model.type="OLS")
  m3 = kclass(formula, data=mroz, k=3, model.type="TSLS")
  expect_match(m$model.type, "KCLASS")
  expect_match(m2$model.type, "KCLASS")
  expect_match(m3$model.type, "KCLASS")
})
test_that("selecting KCLASS without setting k gives an error.", {
  expect_error(kclass(formula, data=mroz, model.type="KCLASS"),
               "If you select KCLASS you must also set k.")
})


