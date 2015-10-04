#' kclass Instrumental variable estimator.
#'
#' \code{kclass} fits instrumental variables models using
#' k-class estimators.
#'
#' K-class estimators are a generic class of instrumental regression
#' estimators. The \code{k} is a parameter that informs the calculation of
#' coefficients. In particular, it informs the weight of the instruments.
#' K-class estimators include two-stage least squares (TSLS) where \code{k}=1,
#' limited information maximum likelihood (LIML) where \code{k} is the minimum
#' eigenvalue of the LIML matrix, and FULLER in which \code{k} is the LIML \code{k} plus an
#' additional \code{alpha} parameter. In principal, \code{k} can take on any value,
#' though values other than those above are not generally used.
#'
#' @param formula formula specification: y ~ X1 + X2 | Z1 + Z2. If the right
#'   side is missing, the returned result is an OLS estimator.
#' @param data a data frame or list containing the variables named in the formula.
#' @param weights an optional vector of weights to be used in estimation.
#' @param model,x,y,z logicals indicating whether to return the model frame,
#'   regressors, response, and instruments used.
#' @param subset optional subset of observations to be used in fitting.
#' @param offset known components with coefficient 1 to be included in the predictor. This
#'   should be NULL or a vector of length equal to the number of cases.
#' @param savefirst logical if the first stage fitted values (or projected
#'   matrix) should be saved.
#' @param ... additional parameters passed to kclass.fit.
#' @return \code{kclass} returns an object of class "kclass". The function
#' \code{summary} is used to print a summary table of results. The generic accessor
#' functions \code{coefficients}, \code{fitted.values}, \code{residuals}, and
#' \code{effects} provide those values.
#'
#' A \code{kclass} object is a list containing the following properties:
#' \tabular{ll}{
#'   \code{call} \tab the matched call used to create the estimators. \cr
#'   \code{coefficients} \tab  a named vector of coefficients. \cr
#'   \code{residuals} \tab the residuals, calculated as the response less fitted values. \cr
#'   \code{fitted.values} \tab the fitted mean values. In OLS this is X'b. \cr
#'   \code{weights} \tab the specified weights. \cr
#'   \code{df.residual} \tab the residual degrees of freedom. \cr
#'   \code{terms} \tab the model \code{\link{terms}} object. \cr
#'   \code{model} \tab if requested, the model frame used. \cr
#'   \code{x} \tab if requested, the endogenous variables used. \cr
#'   \code{y} \tab if requested, the response variable used. \cr
#'   \code{z} \tab if requested, the exogenous instruments used. \cr
#' }
#' @aliases tsls liml fuller iv ivreg
#' @seealso \code{AER::ivreg}
#' @import Formula
#' @export
#' @examples
#' library(rkclass)
#'
#' #mroz
#' data(mroz)
#'
#' # LIML model with interaction of YOB and QOB as instruments
#' model = kclass(LWKLYWGE ~ EDUC + as.factor(YOB) | . - EDUC + interaction(YOB, QOB), data = qob)
#' summary(model)
#' #     Estimate  Std. Error
#' #EDUC 0.089116   0.16110
#' #etc...
#'
#' \dontrun{
#' #This is equivalent to `ivreg` from the AER package:
#' maer = AER::ivreg(LWKLYWGE ~ EDUC + as.factor(YOB) | . - EDUC + interaction(YOB, QOB), data = qob)
#' summary(maer)
#'
#' #And similar to using GMM directly (warning: running this use serious resources!):
#' mgmm = gmm::gmm(lwage ~ exper + expersq + educ, ~ age + kidslt6 + kidsge6, data=mroz)
#' }
kclass <- function(formula, data, ...,
                   weights=NULL, subset=NULL, offset=NULL,
                   model=TRUE, x=FALSE, y=TRUE, z=FALSE, xz=FALSE, savefirst=FALSE) {
  #Get the call for later
  call = match.call()

  if(missing(data)) data = environment(formula)

  if(!is.null(subset)) data = subset(data, subset)

  #set up the model.frame() call, limit to only those variables below
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights", "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE

  ## try to handle dots in formula (from AER package)
  has_dot <- function(formula) inherits(try(terms(formula), silent = TRUE), "try-error")
  formula = as.Formula(formula)
  if(has_dot(formula)) {
    f1 <- formula(formula, rhs = 1)
    f2 <- formula(formula, lhs = 0, rhs = 2)
    if(!has_dot(f1) & has_dot(f2))
      formula <- as.Formula(f1, update(formula(formula, lhs = 0, rhs = 1), f2))
  }

  #make the model.frame() call with updated formula and limited variables
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())

  # Get model frame and response
  Y <- model.response(mf, "numeric")

  #Model terms for X
  modelTerms = terms(formula, data=data)
  modelTerms.X = terms(formula, data=data, rhs=1)
  X <- model.matrix(modelTerms.X, data=mf)

  #Model terms for Z
  if(length(formula)[2] < 2L) {
    modelTerms.Z <- NULL
    Z <- NULL
  } else {
    modelTerms.Z <- delete.response(terms(formula, data = data, rhs = 2))
    Z <- model.matrix(modelTerms.Z, mf, contrasts)
  }

  weights = model.weights(mf)
  offset = model.offset(mf)
  if(is.null(offset)) offset = 0
  if(length(offset) == 1) offset = rep(offset(nrow(Y)))
  offset = as.vector(offset)
  #if(is.null(weights) & !is.null(Z)) weights = diag(dim(Z)[2])

  est = kclass.fit(X, Y, Z, weights, ...)
  est$call = call
  est$formula = formula(formula)
  est$terms = list(regressors = modelTerms.X, instruments = modelTerms.Z, full = modelTerms)
  est$na.action = attr(mf, "na.action")
  est$levels = .getXlevels(modelTerms, mf)
  est$contrasts = list(regressors = attr(X, "contrasts"), instruments = attr(Z, "contrasts"))
  if(model) est$model = mf
  if(x) est$regressors = X
  if(y) est$y = Y
  if(z & !is.null(Z)) est$instruments = Z
  if(xz) est$projected = XZ

  return(est)
}

#' Low-level kclass instrumental variables function.
#'
#' @param x data frame or matrix of endogenous regressors.
#' @param y data frame or vector of the dependent variable.
#' @param z data frame or matrix of instruments and/or exogenous regressors.
#' @param weights sample weights. Default is NULL.
#' @param k the k parameter value. If k is set, \code{model.type} is set to "KCLASS".
#' @param alpha the fuller parameter, only used if \code{model.type} is "FULLER".
#' @param model.type string determining model type. Must be one of "OLS", "LIML",
#'   "TSLS", "KCLASS", "FULLER". Default is LIML."
#' @param eig eigenvalue method. One of "eigen" or "geigen".
#' @param na.action function indicating what to do when data contain NAs. The
#'   default is set by the na.action setting of options
#' @param offset model offset.
#' @param ... additional parameters not currently used.
#' @import geigen
#' @importFrom Matrix crossprod qr
#' @return a model object of class kclass.
#' @export
kclass.fit <- function(x, y,
                      z=NULL, weights=NULL, offset=NULL, k=NULL, alpha=NULL,
                      model.type=c("TSLS", "LIML", "FULLER", "KCLASS", "OLS"),
                      eig=c("eigen","geigen"), na.action, ...) {

  #Sanity Checks
  if(model.type=="KCLASS" & is.null(k)) stop("If you select KCLASS you must also set k.")

  eig = match.arg(eig)
  model.type = match.arg(model.type)

  # ensure as matrices
  if(!is.matrix(x)) x <- as.matrix(x)
  if(!is.matrix(y)) y <- as.matrix(y)
  if(!is.matrix(z) & !is.null(z)) z <- as.matrix(z)

  # sizes of each type of regressor
  N = nrow(y)

  #Set weights stand in / sqrt because we multiply before the QR decomp.
  if(sum((weights < 0)) > 0) stop("Weights must be positive.")
  if(is.null(weights)) w = rep(1,N) else w = sqrt(weights)

  #Offset
  if(is.null(offset)) offset = rep(0,N)

  #if no z, then OLS
  if(is.null(z)) model.type="OLS"


  #Get names of endogenous, exogenous, and instrument variables
  regressors = colnames(x)
  instruments = colnames(z)
  endogenous <- regressors[!(regressors %in% instruments)] #instrumented variables
  excluded_instruments = instruments[!(instruments %in% regressors)]
  exogenous = regressors[regressors %in% instruments] #non-instrumented, also called included instruments.
  #if no z, then all x are endogenous and there are no exogenous regressors
  if(is.null(z)) {
    n.ins = 0
    n.exc = 0
  }
  else {
    n.ins = ncol(z) # included and excluded instruments
    n.exc = length(excluded_instruments) #excluded instruments
  }
  n.reg = ncol(x) #endogenous and exogenous regressors
  n.end = length(endogenous) #endogenous/instrumented regressors
  n.exo = n.reg-n.end #exogenous X variables / included instruments
  n.inc = n.exo #included instruments

  #Main matrix
  A = cbind(x[,exogenous, drop=FALSE],
            z[,excluded_instruments, drop=FALSE],
            x[,endogenous, drop=FALSE],
            y)

  QA = qr(A * w) #multiply by the weights before doing QR

  if(!all(QA$pivot==1:ncol(A))) {
    print("Rank issue...")
    #print(QA$pivot)
    #cat("Rank:\n")
    #print(QA$rank)
    max_piv <- which.max(QA$pivot)
    out_piv <- QA$pivot[(max_piv+1):length(QA$pivot)]
    n_inc <- 1:n.inc
    n_ins <- n.inc + 1:n.ins
    n_end  <- (n.exo + n.ins) + (1:n.end)
    n_y  <- n.exo + n.ins + n.end + 1
    if(any(out_piv %in% n_end)) n_end <- n_end -1
  }

  #Calculate model matrix from which all others are derived
  RA = qr.R(QA) [, order(QA$pivot)]

  #Define 'R' matrices
  #Following Belsley 1974.
  n1 = NULL
  n2 = NULL
  n3 = n.ins + 1:n.end #endogenous variables (X1)
  n4 = n.ins + n.end + 1 #regressand/response (Y)
  if(n.exo > 0) {
    n1 = 1:n.inc #included instruments / exogenous variables (X2)
    R11 = RA[n1, n1, drop=FALSE] # X2
    R13 = RA[n1, n3, drop=FALSE] # X2X1
    R14 = RA[n1, n4, drop=FALSE] # X2Y
  }
  if(n.exc > 0) {
    n2 = n.inc + 1:n.exc #excluded instruments (Z)
    R23 = RA[n2, n3, drop=FALSE] #ZX1
    R24 = RA[n2, n4, drop=FALSE] #ZY
  }
  R33 = RA[n3, n3, drop=FALSE] #X1
  R34 = RA[n3, n4, drop=FALSE] #X1Y

  # Determin k parameter
  # res = cbind(X1,Y) - as.matrix(X2) %*% coef(Mx)
  # coef(Mx) = solve(X2X2) %*% t(X2) %*% X1Y
  # crossprod(X1Y, X1Y) = R34, R34
  # crossprod(R11)
  # crossprod(res) = crossprod(R34) - crossprod(R11) * coef(Mxc)
  if(is.null(k)) k = .set_k(model.type, x[,endogenous], x[, exogenous],
                            z[, excluded_instruments], y, alpha, N, n.ins, eig)
  else model.type = "KCLASS"

  #R-based matrices to solve for coefficients
  if(n.exc > 0 & n.inc > 0) { #we have both X2 and Z
    M = rbind(
      cbind(crossprod(R13) + crossprod(R23) + (1-k)*crossprod(R33), crossprod(R13,R11)),
      cbind(crossprod(R11,R13), crossprod(R11)))
    d = rbind(crossprod(R13,R14) + crossprod(R23,R24) + (1-k)*crossprod(R33,R34), crossprod(R11,R14))

  }
  else if(n.inc>0) { #we have Z and no X2
    M = crossprod(R23) + (1-k)*crossprod(R33)
    d = crossprod(R23,R24) + (1-k)*crossprod(R33,R34)
  }
  else { #We have no X2 and no Z
    M = crossprod(R33)
    d = crossprod(R33,R34)
  }

  #Calculate the coefficients and the covariance matrix
  Minv = qr.solve(qr(M))
  coef =  Minv %*% d
  rownames(coef) = c(endogenous, exogenous)
  colnames(Minv) = rownames(Minv)

  #Calculate auxillary properties
  df.res =  N - n.reg #df
  fitted = A[, c(n3,n1)] %*% coef #fitted values
  res = as.vector(A[,n4]) - fitted #residuals
  dfi = 1

  ###########################
  #Create the returned object
  est = list()

  #settings
  est$model.type = model.type

  #parameters
  est$n = N
  est$nobs = sum(w > 0)
  est$df.residual = df.res
  est$k = k
  est$alpha = alpha
  est$rank = length(coef)
  est$offset = if(identical(offset, rep(0, N))) NULL else offset
  est$weights = weights

  #Results - we reorder the matrices to reflect model terms
  est$coefficients = coef
  names(est$coefficients) = c(endogenous, exogenous)
  est$coefficients = est$coefficients[regressors] #reorder the coefficients
  est$cov.unscaled = drop(Minv)[regressors, regressors] #reorder the cov matrix
  est$fitted.values = drop(fitted)
  est$sigma = as.double(sqrt(crossprod(res * w)/df.res))
  est$residuals = drop(res)

  class(est) = "kclass"
  est
}


