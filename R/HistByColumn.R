#' @title Histogram for each column of a \code{data.frame}
#'
#' @description
#'      Plots a histogram for each numeric column of a \code{data.frame}
#' @details
#'      Searches for numeric columns in the \code{data.frame}, will only produce
#'      a histogram for the numeric ones. You may need to
#'      \code{as.numeric(integers)}. If your \code{data.frame} isn't organsied
#'      so that each row = 1 observation and each column = 1 variable then
#'      you may have to re-arrange your data or write a new function. NaNs
#'      produced by transformation are handled internally.
#'
#' @param x A \code{data.frame} with some numeric columns
#' @param trans A string indicating a transformation of the data to be plotted.
#'      "none" indicates no transformation.
#' @param ... variables passed to other fucntions
#'
#' @return Nothing. Base graphics are used to produce plots.
#'
#' @examples
#'dat <- as.data.frame(matrix(ncol = 20, nrow = 100))
#'for (i in 1:20){
#'        dat[, i] <- rnorm(mean = i, sd = (i/2), n = 100)
#'        }
#'
#'HistByColumn(dat)
#'
#'dat <- cbind(dat, rep("something non-numeric", times = 100))
#'HistByColumn(dat, trans = "log")
#' @export
HistByColumn <- function(x, trans = c("none", "log", "log10", "sqrt"), ...){
        if (class(x) != "data.frame"){stop("x must be of class data.frame")}
        trans <- match.arg(trans)
        foo <- lapply(x, class)
        foof <- which(foo == "numeric")
        for (i in seq_along(foof)){
                j <- foof[i]
                main <- names(x)[j]
                if (trans == "none"){
                        hist(x[, j], main = main, xlab = "value",
                             ylab = "frquency")}
                if (trans == "log10"){
                        suppressWarnings(temp <- log10(x[, j]))
                        temp <- temp[!is.na(temp)]
                        hist(temp, main = paste("log10 ", main),
                             xlab = "value", ylab = "frquency")}
                if (trans == "log"){
                        suppressWarnings(temp <- log(x[, j]))
                        temp <- temp[!is.na(temp)]
                        hist(temp, main = paste("log ", main),
                             xlab = "value", ylab = "frquency")}
                if (trans == "sqrt"){
                        suppressWarnings(temp <- sqrt(x[, j]))
                        temp <- temp[!is.na(temp)]
                        hist(temp, main = paste("sqrt ", main),
                             xlab = "value", ylab = "frquency")}
        }
}

dat <- as.data.frame(matrix(ncol = 20, nrow = 100))
for (i in 1:20){
        dat[, i] <- rnorm(mean = i, sd = (i/2), n = 100)
}
HistByColumn(dat)

dat <- cbind(dat, rep("something non-numeric", times = 100))
HistByColumn(dat, trans = "log")
