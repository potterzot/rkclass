#' Get a component from a model object.
#'
#'  @param object a model object of class kclass.
#'  @param component the component name.
#'  @return a vector or matrix of component values.
.get_component <- function(object, component) {
  if(!is.null(object[[component]])) res = object[[component]]
  else if(!is.null(object$model)) res = model.matrix(object$terms[[component]],
                                                   object$model,
                                                   contrasts = object$contrasts[[component]])
  else stop("must have model matrix or component saved in model object.")
  res
}

#' Get k value based on model type (or, for LIML, on eigenvalue).
#'
#' @param k user set k value.
#' @param model.type One of OLS, TSLS, LIML, FULLER, or KCLASS.
#' @param endogenous the endogenous regressors being instrumented.
#' @param exogenous the exogenous regressors not instrumented.
#' @param instruments excluded instruments.
#' @param y the dependent variable.
#' @param alpha the FULLER parameter.
#' @param N the number of observations.
#' @param L the number of (included and excluded) instruments.
#' @param eig the method to use for eigenvalue computation.
#' @return numeric value of k.
#' @importFrom Matrix crossprod
.set_k <- function(model.type, endogenous = NULL, exogenous = NULL, instruments = NULL, y = NULL,
                   alpha=NULL, N=NULL, L=NULL,
                   eig="eigen") {
  if(model.type=="OLS") {
    #print("model.type is set to OLS. Setting k to 0.")
    k = 0
  }
  else if(model.type=="TSLS") {
    #print("model.type is set to TSLS. Setting k to 1.")
    k = 1
  }
  else if(model.type=="LIML" | model.type=="FULLER") {
    if(missing(endogenous) | missing(exogenous) | missing(instruments) | missing(y))
      stop("To determine k for a LIML or FULLER model we must have endogenous and exogenous
           regressors, instruments, and the dependent variable.")
    xy = cbind(endogenous, y)
    ymxy = crossprod(lm.fit(exogenous, xy)$residuals)
    ymzy = crossprod(lm.fit(instruments, xy)$residuals)

    if(eig=="eigen") k = min(eigen(ymxy %*% solve(ymzy))$values)
    else k = min(geigen(ymxy, ymzy, symmetric=TRUE, only.values=TRUE)$values)

    if(model.type=="FULLER") {
      stopifnot(!is.null(alpha), !is.null(N), !is.null(L))
      k = k-alpha/(N-L)
    }
  }
  #Otherwise, just use the k provided
  k
}

