# John Douglass R & F July 2013
#
#
# HampelFilter.R
# Desc: Script showing derivation(bit grand!) of HampelFilter function from
# outlierMAD() fucntion from pracma package - alteration made to provide more
# flexibility and to allow replacement values to be marked N
#
# Inputs: outlierMAD() from pracma
# Outputs: HampelFilter() fucntion
#
# Library statements
require(pracma)

# view outlierMAD()
outlierMAD()
# actually appears outlierMAD() isn't part of the pracma pacakge - oh well
# there is code on the web

outlierMAD <- function (x, k){
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  t0 <- 3
  for (i in (k + 1):(n - k)) {
    x0 <- median(x[(i - k):(i + k)])
    S0 <- L * median(abs(x[(i - k):(i + k)] - x0))
    if (abs(x[i] - x0) > t0 * S0) {
      y[i] <- x0
      ind <- c(ind, i)
    }
  }
  list(y = y, ind = ind)
}

# improved version - replaces outlier with error value, allows user to alter
# k.multiplier and now handles NAs
HampelFilter <- function (x, k.window, k.multiplier = 3){
  n <- length(x)
  y <- x
  ind <- c()
  NA.ind<- c()
  L <- 1.4826
  k <- k.window
  t0 <- k.multiplier
  NA.ind <- is.na(x)
  x[is.na(x)] <- 0
  for (i in (k + 1):(n - k)) {
    x0 <- median(x[(i - k):(i + k)])
    S0 <- L * median(abs(x[(i - k):(i + k)] - x0))
    if (abs(x[i] - x0) > t0 * S0) {
      y[i] <- -9999
      ind <- c(ind, i)
    }
  }
  # I know, I know...
  for(i in 1:length(NA.ind)){
    if(NA.ind[i] == TRUE){
      y[i] <- NA
    }
  }
  list(y = y, ind = ind)
}

# improved version - replaces outlier with error value, allows user to alter
# k.multiplier and now handles NAs
HampelFilterNonMove <- function (x, k.multiplier = 3){
  n <- length(x)
  y <- x
  ind <- c()
  NA.ind<- c()
  L <- 1.4826
  t0 <- k.multiplier
  NA.ind <- is.na(x)
  x[is.na(x)] <- 0
  for (i in 1:n) {
    x0 <- median(x)
    S0 <- L * (median(abs(x) - x0))
    if (abs(x[i] - x0) > t0 * S0) {
      y[i] <- -9999
      ind <- c(ind, i)
    }
  }
  # I know, I know...
  for(i in 1:length(NA.ind)){
    if(NA.ind[i] == TRUE){
      y[i] <- NA
    }
  }
  list(y = y, ind = ind)
}


