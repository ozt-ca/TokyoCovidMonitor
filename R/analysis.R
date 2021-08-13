#' Fit a model with RStan
#'
#' @param lastday
#' @param num
#'
#' @return out
#' @export
#'
#' @examples
#' fit <- stanBstsFit(100)
stanBstsFit <- function(num = NULL, lastday = NULL){
  # Input today's date and reported number
  nday <- Sys.Date()
  nval <- num
  # Download the raw CSV file
  d <- read.csv('https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv')
  # Extract date, with adding a dummy
  d1 <- data.frame(day = d[, 5], num = 1)

  # Aggregate as daily sum
  d1$day <- as.Date(d1$day) # Put date into Date class
  # Aggregate with group-by
  d2.tmp1 <- dplyr::group_by(d1, day)
  d2.tmp2 <- dplyr::summarise(d2.tmp1, sum(num))
  d2 <- as.data.frame(d2.tmp2)

  names(d2)[2] <- 'num'

  # Set up a consecutive date vector
  dayseq <- seq(from = as.Date(d2$day[1], origin = '1970-01-01'),
                to = as.Date(d2$day[nrow(d2)], origin = '1970-01-01'),
                by = 'day')
  dayseq <- as.data.frame(dayseq)
  names(dayseq) <- 'day'

  # Join daily sum over the date vector
  d3 <- dplyr::left_join(dayseq, d2, by = 'day')
  # Fill NAs by 0
  d3[which(is.na(d3$num)), 2] <- 0
  if(is.null(lastday)){
    # Add the latest value MANUALLY
    d3[(nrow(d3) + 1), 1] <- nday
    d3[nrow(d3), 2] <- nval
    d3$num <- as.numeric(d3$num)
  }

  # Remove duplicates
  d4 <- dplyr::distinct(d3, day, .keep_all = T)

  if(!is.null(lastday)){
    idx <- which(d4$day == lastday)
    d4 <- d4[1:idx, ]
  }

  # Set up Stan env
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)

  # Data as a list
  N <- nrow(d4)
  y <- d4$num
  dat <- list(N = N, y = y)

  # Fit by Stan
  fit <- rstan::sampling(stanmodels$tokbsts, data = dat,
                     iter = 1000, chains = 4, verbose = T)

  # Dataframe by generations
  da1 <- data.frame(day = d[, 5], age = d[, 9], num = 1)
  da1$day <- as.Date(da1$day)
  da2.tmp1 <- dplyr::group_by(da1, day, age)
  da2.tmp2 <- dplyr::summarise(da2.tmp1, sum(num))
  da2 <- as.data.frame(da2.tmp2)
  da3 <- dplyr::left_join(dayseq, da2, by = 'day')
  names(da3)[3] <- 'num'
  da3[which(is.na(da3$num)), 3] <- 0
  da4 <- da3[2045:(nrow(da3)), ]

  out <- list()
  out$df <- d4
  out$fit <- fit
  out$gen.df <- da4
  out$dayseq <- dayseq
  return(out)
}

#' Compute fitted values
#'
#' @param out
#'
#' @return val
#' @export
#'
#' @examples
#' val <- fitValue(out)
fitValue <- function(out){
  # Extract MCMC samples to obtain MLE parameters
  N <- nrow(out$df)

  fit.smp <- rstan::extract(out$fit)
  trend <- rep(0, N)
  for (i in 1:N) {
    tmp <- density(fit.smp$trend[, i])
    trend[i] <- tmp$x[tmp$y == max(tmp$y)]
  }
  trend_h <- rep(0, N)
  for (i in 1:N) {
    tmp <- fit.smp$trend[, i]
    trend_h[i] <- quantile(tmp, probs = 0.8)
  }
  trend_l <- rep(0, N)
  for (i in 1:N) {
    tmp <- fit.smp$trend[, i]
    trend_l[i] <- quantile(tmp, probs = 0.2)
  }
  season <- rep(0, N)
  for (i in 1:N) {
    tmp <- density(fit.smp$season[, i])
    season[i] <- tmp$x[tmp$y == max(tmp$y)]
  }
  # Fitted time series
  fitted <- cumsum(trend) + season

  val <- list()
  val$fitted <- fitted
  val$trend <- trend
  val$trend_h <- trend_h
  val$trend_l <- trend_l
  val$season <- season
  return(val)
}

#' Plot summary, trend, and seasonality
#'
#' @param val
#' @param out
#' @param saveFile
#'
#' @return
#' @export
#'
#' @examples
#' plotOutput(val, out, saveFile = T)
plotOutput <- function(val, out, saveFile = F){
  # Create a title with start and end dates
  df <- out$df
  mtitle <- paste0('Tokyo, daily from ', df$day[1], ' to ', df$day[nrow(df)])

  y <- out$df$num
  pred <- val$fitted
  trend <- val$trend
  season <- val$season
  dayseq <- out$dayseq

  # Plot
  bin <- floor(nrow(dayseq) / 9)
  xval <- seq(1, bin * 9 + 1, by = bin)
  xstr <- as.character(dayseq$day[xval])

  matplot(cbind(y, pred, cumsum(trend)),
          type = 'l', lty = c(1, 3, 1), lwd = c(1, 2, 3), col = c(1, 2, 4),
          ylab = '', main = mtitle, xaxt = 'n')
  legend('topleft',
         legend = c('Reported', '2nd-diff Trend + Seasonality', '2nd-diff Trend'),
         lty = c(1, 3, 1), lwd = c(1, 2, 3), col = c(1, 2, 4))
  axis(side = 1, at = xval, labels = xstr)
  plot(trend, type = 'l', lwd = 2, xaxt = 'n')
  axis(side = 1, at = xval, labels = xstr)
  plot(season, type = 'l', lwd = 2, xaxt = 'n')
  axis(side = 1, at = xval, labels = xstr)

  # Plot for JPG files
  if(saveFile){
    jpeg(filename = 'covid19_fit_summary.jpg', width = 2000, height = 1200)
    matplot(cbind(y, pred, cumsum(trend)),
            type = 'l', lty = c(1, 3, 1), lwd = 2 * c(1, 2, 3), col = c(1, 2, 4),
            ylab = '', main = mtitle, cex.main = 4, cex.axis = 3, xaxt = 'n')
    legend('topleft',
           legend = c('Reported', '2nd-diff Trend + Seasonality', '2nd-diff Trend'),
           lty = c(1, 3, 1), lwd = 2 * c(1, 2, 3), col = c(1, 2, 4), cex = 3)
    axis(side = 1, at = xval, labels = xstr, cex.axis = 2)
    dev.off()

    jpeg(filename = 'covid19_fit_trend.jpg', width = 2000, height = 1200)
    plot(trend, type = 'l', lwd = 4, cex.axis = 3, xaxt = 'n')
    axis(side = 1, at = xval, labels = xstr, cex.axis = 2)
    dev.off()

    jpeg(filename = 'covid19_fit_season.jpg', width = 2000, height = 1200)
    plot(season, type = 'l', lwd = 4, cex.axis = 3, xaxt = 'n')
    axis(side = 1, at = xval, labels = xstr, cex.axis = 2)
    dev.off()
  }
}

#' Plot of cases by generations
#'
#' @param out
#' @param saveFile
#'
#' @return
#' @export
#'
#' @examples
#' plotGens(saveFile = T)
plotGens <- function(out = NULL, saveFile = F){
  if(is.null(out)){
    # Download the original dataset
    d <- read.csv('https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv')
    # Trace each generation
    da1 <- data.frame(day = d[, 5], age = d[, 9], num = 1)
    da1$day <- as.Date((da1$day))
    da2.tmp1 <- dplyr::group_by(da1, day, age)
    da2.tmp2 <- dplyr::summarise(da2.tmp1, sum(num))
    da2 <- as.data.frame(da2.tmp2)

    # Set up a consecutive date vector
    dayseq <- seq(from = as.Date(da2$day[1], origin = '1970-01-01'),
                  to = as.Date(da2$day[nrow(da2)], origin = '1970-01-01'),
                  by = 'day')
    dayseq <- as.data.frame(dayseq)
    names(dayseq) <- 'day'

    da3 <- dplyr::left_join(dayseq, da2, by = 'day')
    names(da3)[3] <- 'num'
    da3[which(is.na(da3$num)), 3] <- 0
    da4 <- da3[2045:(nrow(da3)), ]
  } else {
    da4 <- out$gen.df
  }

  dayseq2 <- unique(da4$day)
  dayseq2 <- as.data.frame(dayseq2)
  names(dayseq2) <- 'day'
  da4_u10s <- da4[da4$age == '10歳未満', ]
  dtmp_u10s <- dplyr::left_join(dayseq2, da4_u10s, by = 'day')
  names(dtmp_u10s)[3] <- 'num'
  dtmp_u10s[which(is.na(dtmp_u10s$num)), 3] <- 0

  bin <- floor(nrow(dayseq2) / 9)
  xval <- seq(1, bin * 9 + 1, by = bin)
  xstr <- as.character(dayseq2$day[xval])

  gtitle <- paste0('Tokyo, daily from ',
                   da4$day[1], ' to ', da4$day[nrow(da4)], ' by generations')
  matplot(cbind(dtmp_u10s[, 3],
                da4[da4$age == '10代', 3],
    　　　　　　da4[da4$age == '20代', 3],
                da4[da4$age == '30代', 3],
                da4[da4$age == '40代', 3],
                da4[da4$age == '50代', 3],
                da4[da4$age == '60代', 3],
                da4[da4$age == '70代', 3],
                da4[da4$age == '80代', 3]),
          type = 'l', lty = 1, xlab = '', ylab = '',
          main = gtitle,
          col = c('#a00000', '#a0a000', 1, 2, 3, 4, 5, 6, '#a0a0a0'),
          xaxt = 'n')
  legend('topleft',
         legend = c('u10s', '10s', '20s', '30s', '40s', '50s',
                    '60s', '70s', '80s'),
         lty = 1, ncol = 2,
         col = c('#a00000', '#a0a000', 1, 2, 3, 4, 5, 6, '#a0a0a0'))
  axis(side = 1, at = xval, labels = xstr)

  if(saveFile){
    jpeg(filename = 'covid19_fit_generation.jpg',
         width = 2000, height = 1200)
    matplot(cbind(dtmp_u10s[, 3],
                  da4[da4$age == '10代', 3],
                  da4[da4$age == '20代', 3],
                  da4[da4$age == '30代', 3],
                  da4[da4$age == '40代', 3],
                  da4[da4$age == '50代', 3],
                  da4[da4$age == '60代', 3],
                  da4[da4$age == '70代', 3],
                  da4[da4$age == '80代', 3]),
            type = 'l', lty = 1, xlab = '', ylab = '',
            main = gtitle, cex.main = 4, cex.axis = 2, lwd = 2,
            col = c('#a00000', '#a0a000', 1, 2, 3, 4, 5, 6, '#a0a0a0'),
            xaxt = 'n')
    legend('topleft',
           legend = c('u10s', '10s', '20s', '30s', '40s', '50s',
                      '60s', '70s', '80s'),
           lty = 1, ncol = 2, cex = 2.5, lwd = 2,
           col = c('#a00000', '#a0a000', 1, 2, 3, 4, 5, 6, '#a0a0a0'))
    axis(side = 1, at = xval, labels = xstr, cex.axis = 2)
    dev.off()
  }
}

#' Texts for reporting on Twitter
#'
#' @param val
#' @param num
#'
#' @return
#' @export
#'
#' @examples
#' textOutput(val, 100)
textOutput <- function(val, num){
  # Obtain variables
  trend <- val$trend
  season <- val$season
  # Set up a description for reporting on Twitter
  ctrend <- cumsum(trend)
  sentence1 <- paste0("本日の東京都の報告数は", num, "名、",
                      "二階差分トレンドの直近3日間の推定値は",
                      signif(rev(ctrend)[3], 3), ", ",
                      signif(rev(ctrend)[2], 3), ", ",
                      signif(rev(ctrend)[1], 3),
                      "、",
                      "7日周期成分の推定値は",
                      signif(rev(season)[7], 3), ", ",
                      signif(rev(season)[6], 3), ", ",
                      signif(rev(season)[5], 3), ", ",
                      signif(rev(season)[4], 3), ", ",
                      signif(rev(season)[3], 3), ", ",
                      signif(rev(season)[2], 3), ", ",
                      signif(rev(season)[1], 3),
                      "、",
                      "トレンドの直近3日間の差分値は",
                      signif(rev(trend)[3], 3), ", ",
                      signif(rev(trend)[2], 3), ", ",
                      signif(rev(trend)[1], 3), "。"
  )

  sentence2 <- paste0("2020年11月1日以降の年代別推移")
  sentence3 <- paste0("東京都の医療リソースの残り容量に関する各指標は画像の通り")
  sentence4 <- paste0("使用した自作Rパッケージはこちら https://github.com/ozt-ca/TokyoCovidMonitor")
  print(sentence1)
  print(sentence2)
  print(sentence3)
  print(sentence4)
}

