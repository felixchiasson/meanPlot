make.summary <- function(df, groupvars, stats, errorbar, measure) {


  df.summary <- ddply(df, groupvars,
                      .fun = function(xx, col) {
                        c(N = length(xx[[col]]),
                          statistic = stats.fun(stats, xx[[col]]),
                          ci = if(errorbar=='CI'){CI.fun(stats, xx[[col]])},
                          se = SE.fun(stats, xx[[col]]))
                      }, measure)

  df.summary <- rename(df.summary, c("statistic"=measure))

  print(df.summary)
  df.summary


}


#########################################################
################## Central Tendences ####################
#########################################################

# Harmonic Mean
hmean <- function(x) { 1 / mean(1/x) }

# Geometric Mean
gmean <- function(x) { (prod(x))^(1/length(x)) }

#########################################################
################# Spread Description ####################
#########################################################

# Median Deviation
mad <- function(x) { median(abs(x-median(x))) }

#########################################################
################# Shape Description #####################
#########################################################

# Fischer skew for small sample (SKNEWNESSu)
sk <- function(x) {
  vrx    <- var(x)
  n      <- length(x)
  skbias <- (1/n) * (sum((x-mean(x))^3)) / ((n-1)/n * vrx)^(3/2)
  sqrt(n*(n-1)) / (n-2) * skbias
}

# Pearson Skew (SKEWNESSp)
pearsk <- function(x) {
  sdx <- sd(x)
  (mean(x)-median(x)) / sdx
}

# Kurtosis for small sample (KURTOSISu)
ku <- function(x) {
  vrx    <- var(x)
  n      <- length(x)
  kubias <- (1/n) * (sum((x-mean(x))^4)) / ((n-1)/n * vrx)^(4/2)
  (n+1) / ((n-2)*(n-3)) * ( (n+1) * (kubias -3) +6)
}


stats.fun <- function(type, x) {

  switch(type,
         "mean" = {
           mean(x)
         },
         "median" = {
           median(x)
         },
         "hmean" = {
           hmean(x)
         },
         "gmean" = {
           gmean(x)
         },
         "var" = {
           var(x)
         },
         "sd" = {
           sd(x)
         },
         "mad" = {
           mad(x)
         },
         "IQR" = {
           IQR(x)
         },
         "SKEWNESSu" = {
           sk(x)
         },
         "SKEWNESSp" = {
           pearsk(x)
         },
         "KURTOSISu" = {
           ku(x)
         },
         stop("Please enter a valid summary statistic. See ?meanPlot for a list.")
  )
}


#########################################################
################### Standard Errors #####################
#########################################################


SE.fun <- function(type, x) {

  switch(type,
         "mean" = {
           sdx = sd(x)
           n = length(x)
           se = sdx / sqrt(n)
           se
         },
         "median" = {
           sdx <- sd(x)
           n   <- length(x)
           se  <- sqrt(pi/2) * sdx / sqrt(n)
           se
         },
         "hmean" = {
           hm2  <- hmean(x)^2
           sd1x <- sd(1/x)
           n    <- length(x)
           se   <- hm2 * sd1x / sqrt(n-1)
           se
         },
         "gmean" = {
           gm   <- gmean(x)
           sdlx <- sd(log(x))
           n    <- length(x)
           se   <- gm * sdlx / sqrt(n-1)
           se
         },
         "var" = {
           varx <- var(x)
           n    <- length(x)
           se   <- varx * sqrt(2/(n-1))
           se
         },
         "sd" = {
           sdx <- sd(x)
           n   <- length(x)
           se  <- sdx / sqrt(2*(n-1))
           se
         },
         "mad" = {
           sdx  <- sd(x)
           n    <- length(x)
           se   <- sqrt(2/pi) * sdx / sqrt(n)
           se
         },
         "IQR" = {
           sdx  <- sd(x)
           n    <- length(x)
           q    <- dnorm(qnorm(0.25))
           se   <- sdx / (2 * sqrt(n) * q)
           se
         },
         "SKEWNESSu" = {
           n    <- length(x)
           se   <- sqrt( (6 * n * (n-1)) / ((n-2)*(n+1)*(n+3)) )
           se
         },
         "SKEWNESSp" = {
           n    <- length(x)
           se   <- sqrt( (pi/2 -1 ) / (n) )
           se
         },
         "KURTOSISu" = {
           n    <- length(x)
           se   <- 2 * SE.fun("SKEWNESSu", x) * sqrt( (n^2-1) / ((n-3)*(n+5)) )
           se
         },
         stop("(SE) Please enter a valid summary statistics. See ?meanPlot for a list.")
  )


}

#########################################################
################# Confidence Intervals ##################
#########################################################

CI.fun <- function(type, x, gamma = 0.95){

  switch(type,
         "mean" = {
           se <- SE.fun(type, x)
           n = length(x)
           tc <- qt( c(1/2-gamma/2, 1/2+gamma/2), n-1)
           ci = mean(x) +se * tc
           ci
         },
         "median" = {
           se <- SE.fun(type, x)
           n  <- length(x)
           tc <- qt( c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
           ci <- median(x) + se * tc
           ci
         },
         "hmean" = {
           se <- SE.fun(type, x)
           n  <- length(x)
           tc <- qt( c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
           ci <- hmean(x) + se * tc
           ci
         },
         "gmean" = {
           se <- SE.fun(type, x)
           n  <- length(x)
           tc <- qt( c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
           ci <- gmean(x) + se * tc
           ci
         },
         "var" = {
           varx <- var(x)
           n    <- length(x)
           c2c  <- qchisq( c(1/2+gamma/2, 1/2-gamma/2), n-1)
           ci   <- varx * (n-1) / c2c
           ci
         },
         "sd" = {
           sdx <- sd(x)
           n   <- length(x)
           c2c <- sqrt(qchisq( c(1/2+gamma/2, 1/2-gamma/2), n-1))
           ci  <- sdx * sqrt(n-1) / c2c
           ci
         },
         "mad" = {
           se <- SE.fun(type,x)
           n  <- length(x)
           tc <- qt( c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
           ci <- mad(x) + se * tc
           ci
         },
         "IQR" = {
           se <- SE.fun(type, x)
           n  <- length(x)
           tc <- qt( c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
           ci <- IQR(x) + se * tc
           ci
         },
         "SKEWNESSu" = {
           se <- SE.fun(type, x)
           zc <- qnorm( c(1/2 - gamma/2, 1/2 + gamma/2) )
           ci <- sk(x) + se * zc
           ci
         },
         "SKEWNESSp" = {
           se <- SE.fun(type, x)
           n  <- length(x)
           tc <- qt( c(1/2 - gamma/2, 1/2 + gamma/2), n-1 )
           ci <- pearsk(x) + se * tc
           ci
         },
         "KURTOSISu" = {
           n    <- length(x)
           minbx <- 2 * (n-1) / (n-3)
           se <- SE.fun(type, x)
           lnc <- qlnorm( c(1/2 - gamma/2, 1/2 + gamma/2) )
           ci <- ku(x) + 2 * lnc ^ (se / 2) - minbx
           ci
         },
         stop("(CI) Please enter a valid summary statistic. See ?meanPlot for a list.")
  )

}
