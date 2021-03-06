% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/kclass.R
\name{kclass}
\alias{fuller}
\alias{iv}
\alias{ivreg}
\alias{kclass}
\alias{liml}
\alias{tsls}
\title{kclass Instrumental variable estimator.}
\usage{
kclass(formula, data, ..., weights = NULL, subset = NULL, offset = NULL,
  model = TRUE, x = FALSE, y = TRUE, z = FALSE, xz = FALSE,
  savefirst = FALSE)
}
\arguments{
\item{formula}{formula specification: y ~ X1 + X2 | Z1 + Z2. If the right
side is missing, the returned result is an OLS estimator.}

\item{data}{a data frame or list containing the variables named in the formula.}

\item{...}{additional parameters passed to kclass.fit.}

\item{weights}{an optional vector of weights to be used in estimation.}

\item{subset}{optional subset of observations to be used in fitting.}

\item{offset}{known components with coefficient 1 to be included in the predictor. This
should be NULL or a vector of length equal to the number of cases.}

\item{model,x,y,z}{logicals indicating whether to return the model frame,
regressors, response, and instruments used.}

\item{savefirst}{logical if the first stage fitted values (or projected
matrix) should be saved.}
}
\value{
\code{kclass} returns an object of class "kclass". The function
\code{summary} is used to print a summary table of results. The generic accessor
functions \code{coefficients}, \code{fitted.values}, \code{residuals}, and
\code{effects} provide those values.

A \code{kclass} object is a list containing the following properties:
\tabular{ll}{
  \code{call} \tab the matched call used to create the estimators. \cr
  \code{coefficients} \tab  a named vector of coefficients. \cr
  \code{residuals} \tab the residuals, calculated as the response less fitted values. \cr
  \code{fitted.values} \tab the fitted mean values. In OLS this is X'b. \cr
  \code{weights} \tab the specified weights. \cr
  \code{df.residual} \tab the residual degrees of freedom. \cr
  \code{terms} \tab the model \code{\link{terms}} object. \cr
  \code{model} \tab if requested, the model frame used. \cr
  \code{x} \tab if requested, the endogenous variables used. \cr
  \code{y} \tab if requested, the response variable used. \cr
  \code{z} \tab if requested, the exogenous instruments used. \cr
}
}
\description{
\code{kclass} fits instrumental variables models using
k-class estimators.
}
\details{
K-class estimators are a generic class of instrumental regression
estimators. The \code{k} is a parameter that informs the calculation of
coefficients. In particular, it informs the weight of the instruments.
K-class estimators include two-stage least squares (TSLS) where \code{k}=1,
limited information maximum likelihood (LIML) where \code{k} is the minimum
eigenvalue of the LIML matrix, and FULLER in which \code{k} is the LIML \code{k} plus an
additional \code{alpha} parameter. In principal, \code{k} can take on any value,
though values other than those above are not generally used.
}
\examples{
library(rkclass)

#mroz
data(mroz)

# LIML model with interaction of YOB and QOB as instruments
model = kclass(LWKLYWGE ~ EDUC + as.factor(YOB) | . - EDUC + interaction(YOB, QOB), data = qob)
summary(model)
#     Estimate  Std. Error
#EDUC 0.089116   0.16110
#etc...

\dontrun{
#This is equivalent to `ivreg` from the AER package:
maer = AER::ivreg(LWKLYWGE ~ EDUC + as.factor(YOB) | . - EDUC + interaction(YOB, QOB), data = qob)
summary(maer)

#And similar to using GMM directly (warning: running this use serious resources!):
mgmm = gmm::gmm(lwage ~ exper + expersq + educ, ~ age + kidslt6 + kidsge6, data=mroz)
}
}
\seealso{
\code{AER::ivreg}
}

