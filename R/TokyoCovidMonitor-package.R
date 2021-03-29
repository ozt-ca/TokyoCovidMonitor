#' The 'TokyoCovidMonitor' package.
#'
#' @description This package offers a simple modeling of Bayesian
#' structure time series with RStan, for monitoring daily COVID-19
#' positive cases in Tokyo.Currently, Tokyo metropolitan government
#' announces the number of COVID-19 positive case at 3 pm on a daily basis.
#' The package enables you to grasp the latest time-series trend of
#' daily positive cases, just with inputting the number announced.
#' Unlike the other advanced models such as SIR or combination of
#' SIR and some machine learning, the model of this package is quite simple.
#' But it shows a simple daily trend that can be easily interpreted,
#' and the current situation in which the number of cases is increasing
#' or decreasing.
#'
#' @docType package
#' @name TokyoCovidMonitor-package
#' @aliases TokyoCovidMonitor
#' @useDynLib TokyoCovidMonitor, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. https://mc-stan.org
#'
NULL
