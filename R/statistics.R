#' @title Summarize data
#'
#' @description Summarizes data using summary statistic of choice and returns a data frame sorted by group.
#'
#' @param df Data Frame
#' @param groupvars Name of the column(s) containing your bsfactor and wsfactor
#' @param stats Summary Statistic to use, see documentation for a list.
#' @param width Type of error bar ("SE" or "CI")
#' @param measure The name of the column containing your dependent variable
#'
#' @return data.frame
#'
#' @examples make_summary(ToothGrowth, groupvars = c("supp", "dose"), stats = "mean", width = "CI", measure = "len")

make_summary <- function(df, groupvars, stats, width, measure, ...) {

  wfct <- paste(width, stats, sep = ".")
  df.summary <- plyr::ddply(df, groupvars,
                      .fun = function(xx, col) {
                        c(N = length(xx[[col]]),
                          statistic = do.call(stats, list(xx[[col]])),
                          error = do.call(wfct, list(xx[[col]], ...)))
                      }
                      , measure)

  df.summary[groupvars] <- lapply(df.summary[groupvars], as.factor)

  df.summary


}

########################################################
###################### Adjustments #####################
########################################################

purpose <- function(x, width) {
  (x[grepl(width, names(x))] - ifelse(width == "CI", x["statistic"], 0)) *
    sqrt(2) + ifelse(width == "CI", x["statistic"], 0)
}

pop.adjust <- function(x, width, n) {
  (x[grepl(width, names(x))] - ifelse(width == "CI", x["statistic"], 0)) *
    sqrt(1 - x$N / n) + ifelse(width == "CI", x["statistic"], 0)
}


########################################################
################## CENTRAL TENDENCIES ##################
########################################################

######################### MEAN #########################
SE.mean <- function(x){
  sdx <- sd(x)
  n   <- length(x)
  se  <- sdx / sqrt(n)
  se
}
CI.mean <- function(x, gamma = 0.95){
  se <- SE.mean(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- mean(x) + se * tc
  ci
}



######################## MEDIAN ########################
SE.median <- function(x){
  sdx <- sd(x)
  n   <- length(x)
  se  <- sqrt(pi/2) * sdx / sqrt(n)
  se
}
CI.median <- function(x, gamma = 0.95){
  se <- SE.median(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- median(x) + se * tc
  ci
}


#################### HARMONIC MEAN #####################
hmean <- function(x) { 1 / mean(1/x) }
SE.hmean <- function(x){
  hm2  <- hmean(x)^2
  sd1x <- sd(1/x)
  n    <- length(x)
  se   <- hm2 * sd1x / sqrt(n-1)
  se
}
CI.hmean <- function(x, gamma = 0.95){
  se <- SE.hmean(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- hmean(x) + se * tc
  ci
}


#################### GEOMETRIC MEAN ####################
gmean <- function(x) { (prod(x))^(1/length(x)) }
SE.gmean <- function(x){
  gm   <- gmean(x)
  sdlx <- sd(log(x))
  n    <- length(x)
  se   <- gm * sdlx / sqrt(n-1)
  se
}
CI.gmean <- function(x, gamma = 0.95) {
  se <- SE.gmean(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- gmean(x) + se * tc
  ci
}


########################################################
################## SPREAD DESCRIPTION ##################
########################################################


####################### VARIANCE #######################
SE.var <- function(x){
  varx <- var(x)
  n    <- length(x)
  se   <- varx * sqrt(2/(n-1))
  se
}
CI.var <- function(x, gamma = 0.95){
  varx <- var(x)
  n    <- length(x)
  c2c  <- qchisq( c(1/2 + gamma/2, 1/2 - gamma/2), n-1)
  ci   <- varx * (n-1) / c2c
  ci
}


################## STANDARD DEVIATION ##################
SE.sd <- function(x){
  sdx <- sd(x)
  n   <- length(x)
  se  <- sdx / sqrt(2*(n-1))
  se
}
CI.sd <- function(x, gamma = 0.95){
  sdx <- sd(x)
  n   <- length(x)
  c2c <- sqrt(qchisq(c(1/2+gamma/2, 1/2-gamma/2), n-1))
  ci  <- sdx * sqrt(n-1) / c2c
  ci
}


################### MEDIAN DEVIATION ###################
mad <- function(x) {
  median(abs(x-median(x)))
}
SE.mad <- function(x){
  sdx  <- sd(x)
  n    <- length(x)
  se   <- sqrt(2/pi) * sdx / sqrt(n)
  se
}
CI.mad <- function(x, gamma = 0.95){
  se <- SE.mad(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- mad(x) + se * tc
  ci
}


####################### QUANTILE #######################
# quantile function may not work...

################## INTERQUARTILE RANGE##################
SE.IQR <- function(x){
  sdx  <- sd(x)
  n    <- length(x)
  q    <- dnorm(qnorm(0.25))
  se   <- sdx / (2 * sqrt(n) * q)
  se
}
CI.IQR <- function(x, gamma = 0.95){
  se <- SE.IQR(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- IQR(x) + se * tc
  ci
}



########################################################
################### SHAPE DESCRIPTION ##################
########################################################

####################### SKEWNESSu ######################
# this is Fisher skew for small sample
sk <- function(x) {
  vrx <- var(x)
  n   <- length(x)
  skbias <- (1/n) * (sum((x - mean(x))^3)) / ((n-1)/n * vrx)^(3/2)
  sqrt(n * (n-1)) / (n-2) * skbias
}
SE.sk <- function(x){
  n    <- length(x)
  se   <- sqrt( (6 * n * (n - 1)) / ((n - 2) * (n + 1)*(n + 3)) )
  se
}
CI.sk <- function(x, gamma = 0.95){
  se <- SE.sk(x)
  zc <- qnorm(c(1/2 - gamma/2, 1/2 + gamma/2))
  ci <- sk(x) + se * zc
  ci
}



###################### SKEWNESSp #####################
# this is pearson skew
pearsk <- function(x) {
  sdx <- sd(x)
  (mean(x) - median(x)) / sdx
}
SE.pearsk <- function(x){
  n    <- length(x)
  se   <- sqrt( (pi/2 - 1) / n )
  se
}
CI.pearsk <- function(x, gamma = 0.95){
  se <- SE.pearsk(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- pearsk(x) + se * tc
  ci
}


####################### KURTOSISu ######################
# this is kurtosis for small sample
ku <- function(x) {
  vrx <- var(x)
  n   <- length(x)
  kubias <- (1/n) * (sum((x - mean(x))^4)) / ((n - 1)/n * vrx)^(4/2)
  (n+1) / ((n - 2) * (n - 3)) * ((n + 1) * (kubias - 3) + 6)
}
SE.ku <- function(x){
  n    <- length(x)
  se   <- 2 * SE.sk(x) * sqrt( (n^2 - 1) / ((n - 3) * (n + 5)) )
  se
}
CI.ku <- function(x, gamma = 0.95){
  n    <- length(x)
  minbx <- 2 * (n - 1) / (n - 3)
  se <- SE.ku(x)
  lnc <- qlnorm( c(1/2 - gamma/2, 1/2 + gamma/2) )
  ci <- ku(x) + 2 * lnc ^ (se / 2) - minbx
  ci
}
