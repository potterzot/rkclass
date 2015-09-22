library("rkclass")
context("Test S3 method functions for class 'kclass'.")

#Methods to test:
#print.kclass [DONE]
#print.summary.kclass
#summary.kclass [DONE]
#hatvalues.kclass
#vcov.kclass [DONE]
#anova.kclass
#model.matrix.kclass
#predict.kclass [DONE]
#fitted.kclass [DONE]

#Data and model specification for tests
data(mroz)
formula = lwage ~ exper + expersq + educ | . - educ + age + kidslt6 + kidsge6
w = abs(rnorm(753))
m = kclass(formula, data=mroz, model.type="TSLS")
m_aer = AER::ivreg(formula, data=mroz)
mw = kclass(formula, data=mroz, model.type="TSLS", weights=w)
mw_aer = AER::ivreg(formula, data=mroz, weights=w)

#Print
test_that("print.kclass prints model call and results.", {
  printed_m = print(m)
  expect_equal(printed_m, m)
})

#Summary
test_that("summary.kclass produces correct summary statistics", {
  sm = summary(m)
  sm_aer = summary(m_aer, diagnostics = TRUE)
  expect_equal(sm$coefficients, sm_aer$coefficients) #wald test of coefficients
  #expect_equal(sm$wuhausman) #endogeneity
  #expect_equal(sm$sargan) #overidentifying restrictions

})

#vcov
test_that("vcov is correct variance-covariance matrix and works with sandwich.", {
  library(sandwich)
  expect_less_than(vcov(m) - vcov(m_aer), 0.00001)
  expect_less_than(vcovHC(m) - vcovHC(m_aer), 0.00001)
  expect_less_than(vcovHAC(m) - vcovHC(m_aer), 0.00001)
  expect_less_than(vcovHC(m, type="HC0") - vcovHC(m_aer, type="HC0"), 0.00001)
})
test_that("vcov is correct with weighted models.", {
  library(sandwich)
  expect_less_than(vcov(mw) - vcov(mw_aer), 0.00001)
  expect_less_than(vcovHC(mw) - vcovHC(mw_aer), 0.00001)
  expect_less_than(vcovHAC(mw) - vcovHC(mw_aer), 0.00001)
  expect_less_than(vcovHC(mw, type="HC0") - vcovHC(mw_aer, type="HC0"), 0.00001)
})

#vcov_alternatives
test_that("vcov_bekker produces the correct values.", {
  skip("Not yet implemented.")
})
test_that("vcov_cse produces correct values.", {
  skip("Not yet implemented.")
})

#predict
test_that("predict yields vector of prediction values.", {
  mroz_new = mroz
  mroz_new$educ = mroz_new$educ + 3 * sin(1:753)
  p = predict(m, newdata = mroz_new)
  p_aer = predict(m_aer, newdata = mroz_new)
  pw = predict(mw, newdata = mroz_new)
  pw_aer = predict(mw_aer, newdata = mroz_new)
  expect_equal(p, p_aer)
  expect_equal(pw, pw_aer)
})

#model.matrix
test_that("model.matrix returns correct matrix.", {
  mm = model.matrix(m)
  mm_aer = model.matrix(m_aer)
  expect_equal(mm, mm_aer)
  expect_equal(nrow(mm), 428)
  expect_equal(ncol(mm), 4)
  expect_equal(sum(mm[,1]), 428)
  expect_equal(sum(mm[,2]), 5580)
  expect_equal(sum(mm[,3]), 100460)
  expect_equal(sum(mm[,4]), 5418)
})

#fitted
test_that("fitted model is correct.", {
  f = fitted(m)
  f_base = fitted(m_aer)
  fw = fitted(mw)
  fw_base = fitted(mw_aer)
  expect_equal(fw, fw_base)
})
