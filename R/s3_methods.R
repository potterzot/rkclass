#' S3 method for extracting the 'bread' portion of a sandwich estimator.
#'
#' @param object the fitted kclass model object.
#' @param ... additional parameters.
#' @return A matrix containing an estimator for the expectation of the negative
#'   derivative of the estimating functions, usually a Hessian matrix.
#' @seealso sandwich
#' @export
bread.kclass <- function(object, ...) {
  object$cov.unscaled * object$nobs
}

#' S3 method for a kclass estimator function
#'
#' @param object the fitted kclass model object.
#' @param ... other parameters passed to methods.
#' @return A matrix containing the empirical estimating functions.
#' @seealso estfun
#' @export
estfun.kclass <- function(object, ...) {
  X = model.matrix(object)
  w = weights(object)
  if(is.null(w)) w = 1
  res = residuals(object)
  ret = as.vector(res) * w * X
  attr(ret, "assign") = NULL
  attr(ret, "contrasts") = NULL
  ret
}

#' S3 method to get the hat values for a model.
#'
#' @param object a fitted model of type kclass.
#' @param full logical to return the full matrix or just the diagonal.
#' @return a diagonal (or matrix) of estimated values useful in alternative
#' @export
hatvalues.kclass <- function(object, full=FALSE) {
  xz = model.matrix(object, component="projected")
  x = model.matrix(object, component="regressors")
  z = model.matrix(object, component="instruments")
  Pz = z %*% chol2inv(qr.R(qr(z))) %*% t(z)
  Pzx = x %*% chol2inv(qr.R(qr(xz))) %*% t(x)
  hatv = drop(Pzx) %*% drop(Pz)
  if(!full) hatv = diag(hatv)
  hatv
}

#' S3 method to extract model matrix from kclass objects.
#'
#' @param object a model of class kclass.
#' @param component indicator for regressors, instruments, or projected values.
#' @return the model matrix of the object.
#' @export
model.matrix.kclass <- function(object, component = c("projected", "regressors", "instruments")) {
  component = match.arg(component)

  if(component != "projected")
    res = .get_component(object, component)
  else {
    w = weights(object)
    X = .get_component(object, "regressors")
    Z = .get_component(object, "instruments")
    if(is.null(Z)) res = X
    else res = if(is.null(w)) lm.fit(Z, X)$fitted.values else lm.wfit(Z, X, w)$fitted.values
  }
  res
}

#' Predict values based on a fitted kclass model
#'
#' Generates a vector of outcome predictions based on new regressor data. The
#' new data must be a data frame or matrix with the same number of columns as
#' coefficients in the model.
#'
#' @param object a model object of class 'kclass'.
#' @param newdata a data frame or vector. If no data is submitted, the original
#'   fitted values are returned.
#' @return a vector of predicted values.
#' @export
predict.kclass <- function(object, newdata, na.action=na.pass, ...) {
  if(missing(newdata)) fitted(object)
  else{
    mf <- model.frame(delete.response(object$terms$full), newdata,
                      na.action = na.action, xlev = object$levels)
    X <- model.matrix(delete.response(object$terms$regressors), mf,
                      contrasts = object$contrasts$regressors)
    drop(X %*% object$coefficients)
  }
}

#' S3 print method for class 'kclass'
#'
#' Formats the object and prints some information when the object is entered
#' by itself in the console.
#'
#' @param object the model object returned by \code{kclass()}.
#' @param digits number of digits to display.
#' @param print.gap the gap between each printed coefficient.
#' @param quote logical indicating whether to quote the coefficient values.
#' @param ... other parameters passed to \code{print.default}.
#' @return the object.
#' @export
print.kclass <- function(object,
                         digits = 3, print.gap = 2, quote = FALSE,
                         ...) {
  cat("\nCall:\n", deparse(object$call), "\n", sep="")
  cat("\nCoefficients:\n")
  print.default(format(coef(object), digits = digits), print.gap = print.gap, quote = quote, ...)
  cat("\n")
  invisible(object)
}

#' S3 print method for class 'summary.kclass'
#'
#' @param object an object with class 'summary.kclass', usually ouput from \code{summary()}.
#' @param digits the number of digits to display.
#' @param signif.stars logical to show significance indicators.
#' @param ... other parameters passed to \code{\link{printCoefmat}}.
#' @return the summary object.
#' @importFrom lmtest coeftest
#' @export
print.summary.kclass <- function(object, digits=3, signif.stars=getOption("show.signif.stars"), ...) {

  #Object call
  cat("Call:\n")
  print(object$call)
  cat("\n")

  #Distribution of residuals
  cat(if(!is.null(object$weights) && diff(range(object$weights))) "Weighted ", "Residuals:\n", sep="")
  if(length(object$residuals) > 5L) {
    col_names = c("Min", "1Q", "Median", "3Q", "Max")
    rq = if(length(dim(object$residuals)) == 2) {
      structure(apply(t(object$residuals), 1, quantile),
                dimnames = list(col_names, dimnames(object$residuals)[[2]]))
    }
    else {
      structure(quantile(object$residuals), names = col_names)
    }
    print(rq, digits=digits, ...)
  }
  else {
    print(object$residuals, digits=digits, ...)
  }

  #Coefficients and test
  cat("\nCoefficients:\n")
  printCoefmat(object$coefficients, digits=digits, signif.stars = signif.stars, signif.legend = signif.stars, na.print = "NA", ...)

  invisible(object)
}

#' S3 summary method for 'kclass' objects
#'
#' @param object a model object of class 'kclass'
#' @param vcov. a function or matrix.
#' @param df the degrees of freedom.
#' @param robust logical to report robust sandwich estimator standard errors.
#'   These are heteroskedastic robust.
#' @param errors convenience method for calculating adjusted standard errors.
#'   Use \code{robust} for heterskedastic-robust errors, \code{bw} for autocorrelation-
#'   robust errors, \code{white} for White's estimator. \code{bekker} provides Bekker's
#'   adjusted standard errors (see Bekker 1994), while CSE provides the
#'   corrected standard errors of Hansen, Hausman, and Newey 2004. Other
#'   allowed options include any of the \code{type} options available in
#'   \code{\link{sandwich}{vcovHC}}.
#' @param bw logical to report XX standard errors. These are autocorrelation
#'   robust. Setting both robust and bw to \code{TRUE} reports standard errors
#'   that are both heteroskedastic and autocorrelation robust.
#' @param ... other parameters passed to print function.
#'
#' @return a list object containing the following values:
#' \tabular{ll}{
#'   \code{value} \tab explanation
#' }
#' @importFrom sandwich vcovHC
#' @export
summary.kclass <- function(object, vcov. = NULL, df = NULL,
                           errors = c("standard","const", "robust", "bw", "white", "HAC", "CSE",
                                      "HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"),
                           ...) {
  errors = match.arg(errors)

  #residuals
  res = object$residuals
  y = if(!is.null(object$y)) object$y else object$fitted.values + res
  n = length(res)
  w = if(!is.null(object$weights)) object$weights else rep(1,n)
  res = res * sqrt(w)

  #df
  if(is.null(df)) df=object$df.residual
  if(!is.finite(df)) df = 0
  if(df > 0 & (df != object$df.residual)) df = obect$df.residual

  #model goodness of fit
  rss = crossprod(res)
  if(attr(object$terms$regressors, "intercept")) {
    tss = crossprod(w * (y - weighted.mean(y, w)))
    dfi = 1
  }
  else {
    tss = crossprod(w,y)
    dfi = 0
  }
  r.squared = 1 - rss/tss
  adj.r.squared = 1 - (1 - r.squared) * ((n - dfi) / object$df.residual)

  #variance covariance matrix
  if(is.null(vcov.)) {
    vc = switch(errors,
           standard = vcov(object),
           robust = vcovHC(object, type="HC3"),
           white = vcovHC(object, type="HC0"),
           bw = vcovHAC(object),
           HAC = kernHAC(object),
           bekker = vcov_Bekker(object),
           CSE = vcov_CSE(object),
           vcovHC(object, type=errors) #default if no other options
           )
  }
  else if(is.function(vcov.)) vc = vcov.(object)
  else vc = vcov.

  #Coefficient tests
  cf = coeftest(object, vcov. = vc, df = df, ...)
  attr(cf, "method") = NULL
  class(cf) = "matrix"

  #returned object
  ret = list(
    call = object$call,
    terms = object$terms,
    weights = object$weights,
    residuals = res,
    coefficients = cf,
    sigma = object$sigma,
    df = c(object$rank, if(df>0) df else Inf, object$rank),
    r.squared = r.squared,
    adj.r.squared = adj.r.squared,
    vcov = vc
  )
  class(ret) <- "summary.kclass"
  ret
}

#' S3 terms method for class 'kclass'
#'
#' @param object a model object of class kclass.
#' @param component either "regressors" or "instruments".
#' @param ... other parameters not yet used.
#' @return model terms for component.
terms.kclass <- function(object, component=c("regressors", "instruments"), ...) {
  object$terms[[match.arg(component)]]
}

#' S3 vcov method for class 'kclass'
#'
#' Calculates the variance-covariance matrix for a kclass model.
#'
#' @param object a model object of class 'kclass'.
#' @return a k * k matrix of variances and covariances, where k is the
#'   number of coefficients in the model.
#' @importFrom sandwich vcovHC
#' @export
vcov.kclass <- function(object, vcov. = NULL, errors = NULL) {
  object$sigma^2 * object$cov.unscaled
}




