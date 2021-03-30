# R Package TokyoCovidMonitor
A simple RStan-based package for monitoring daily COVID-19 positive cases in Tokyo

## Overview

<img src = "tools/summary_image.png">

This package offers a simple modeling of Bayesian structure time series with RStan, for monitoring daily COVID-19 positive cases in Tokyo.

Currently, Tokyo metropolitan government announces the number of COVID-19 positive case at 3 pm [here](https://www.fukushihoken.metro.tokyo.lg.jp/) on a daily basis. `TokyoCovidMonitor` enables you to grasp the latest time-series trend of daily positive cases, just with inputting the number announced.

Unlike the other advanced models such as SIR / SEIR or combination of SIR / SEIR with some machine learning, the model of this package is quite simple. But it shows a simple daily trend that can be easily interpreted, and the current situation in which the number of cases is increasing or decreasing.

**Warning**

Please do not use `TokyoCovidMonitor` for predicting future values (daily positive cases). In general, predicting future values of time series is difficult due to various kinds of theoretical limitations. This package offers "as is" perspective, not "to be". If you are interested in prediction, please consider for building your own model based on mathematical epidemiology such as SIR or SEIR, with some other datasets if possible.

We assume that this package is mainly used for people in Tokyo, so some outputs are written in Japanese.

## Installation

Please just run as below. This package won't be available on CRAN.

```
devtools::install_github('ozt-ca/TokyoCovidMonitor')
```

We highly recommend to install `rstan` and related packages in advance, in order to avoid potential errors (e.g. errors related to `Rcpp`) or conflicts.

For some reasons, we have only checked Mac OS environment. It may not work on Windows / Linux / Chrome OS.

## Walk through

If Tokyo metropolitan government announced that the number of reported positive cases today was 100, just run as below.

```
library(TokyoCovidMonitor)
out <- stanBstsFit(100)
val <- fitValue(out)
plotOutput(val, out)
```

If you want to see the past model on such as 2021-01-01, run as below.
```
out <- stanBstsFit(lastday = "2021-01-01")
```

We would be happy if you send us any pull request or feedback.
