#' Weekly earnings, education, and age.
#'
#' A dataset containing 1980 census data used in Angrist and Krueger 1991.
#' The data include Weekly earnings, education, age, year of birth and
#' quarter of birth.
#' @format a data frame with 329508 rows and 6 columns:
#' \tabular{ll}{
#'  \code{LWKLYWGE} \tab Log weekly wage in dollars. \cr
#'  \code{EDUC} \tab Years of education. \cr
#'  \code{AGE} \tab Age in years. \cr
#'  \code{AGESQ} \tab Age squared. \cr
#'  \code{YOB} \tab The last two digist of year of birth. \cr
#'  \code{QOB} \tab Quarter of birth. \cr
#'}
#' @source \url{http://economics.mit.edu/faculty/angrist/data1/data/angkru1991}
"qob"

#' U.S. Women's Labor-Force Participation, 1975
#'
#' This is a common dataset used in statistical evaluation in numerous cases.
#' It is especially common in Stata examples.
#'
#' @format a data frame with 753 rows and 23 columns.
#' \tabular{ll}{
#'   \code{id} \tab Row number / record id. \cr
#'   \code{inlf} \tab 1 if in labor force. \cr
#'   \code{hours} \tab Hours worked. \cr
#'   \code{kidslt6} \tab Number of kids less than 6 years old. \cr
#'   \code{kidsge6} \tab Number of kids greater than or equal to 6 years old. \cr
#'   \code{age} \tab Age in years. \cr
#'   \code{educ} \tab Years of schooling. \cr
#'   \code{wage} \tab Estimated wage from earnings and hours. \cr
#'   \code{repwage} \tab Reported wage in 1976 interview. \cr
#'   \code{hushrs} \tab Hours worked by husband. \cr
#'   \code{husage} \tab Age of husband in years. \cr
#'   \code{huseduc} \tab Husband's years of schooling. \cr
#'   \code{huswage} \tab Husband's wage estimated from earnings and hours. \cr
#'   \code{faminc} \tab Family income in dollars. \cr
#'   \code{mtr} \tab Federal marginal tax rate facing the woman. \cr
#'   \code{motheduc} \tab Woman's mother's years of schooling. \cr
#'   \code{fatheduc} \tab Woman's father's years of schooling. \cr
#'   \code{unem} \tab Unemployment rate in county of residence. \cr
#'   \code{city} \tab 1 if living in the SMSA. \cr
#'   \code{exper} \tab Labor market experience in years. \cr
#'   \code{nwifeinc} \tab Calculated as \deqn{(faminc - wage * hours) / 1000} \cr
#'   \code{lwage} \tab log wage. \cr
#'   \code{expersq} \tab Years of experience squared. \cr
#' }
#' @source Mroz, T.A. (1987) The sensitivity of an empirical model of married
#' women's hours of work to economic and statistical assumptions. \emph{Econometrica}
#' \strong{55}, 765-799.
"mroz"





