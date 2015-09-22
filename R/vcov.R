#' Calculate the Bekker-adjusted variance-covariance matrix.
#'
#' @param object a fitted model of class kclass or ivreg.
#' @return a variance-covariance matrix using Bekker's estimator.
#'   See Bekker 1994 for more details.
#' @references \cite{HansenHausmanNewye2004}
#' @importFrom sandwich sandwich
#' @export
vcov_bekker <- function(object, ...) {
  omega = function(residuals, diaghat, df) {
    residuals^2 #TODO: fix this omega function
  }
  #vcovHC(object, omega=omega)
  stop("Not yet implemented.")
}

#' Calculate corrected standard errors (CSEs)
#'
#' @param object a fitted model of class kclass or ivreg.
#' @return a variance covariance matrix using the CSE estimator proposed in
#'   Hansen, Hausman, and Newey 2004.
#' @importFrom sandwich sandwich
#' @export
vcov_cse <- function(object, ...) {
  omega = function(residuals, diaghat, df) {
    residuals^2 #TODO: fix this omega function
  }
  #vcovHC(object, omega=omega)
  stop("Not yet implemented.")
}
