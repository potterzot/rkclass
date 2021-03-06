% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/vcov.R
\name{vcov_cse}
\alias{vcov_cse}
\title{Calculate corrected standard errors (CSEs)}
\usage{
vcov_cse(object, ...)
}
\arguments{
\item{object}{a fitted model of class kclass or ivreg.}
}
\value{
a variance covariance matrix using the CSE estimator proposed in
  Hansen, Hausman, and Newey 2004.
}
\description{
Calculate corrected standard errors (CSEs)
}

