#' @title Custom year generator
#'
#' @description
#'      Returns a custom year type for example crop year or financial year.
#' @details
#'      Give it two vectors (year and month) and it will give you a character
#'      vector in the format "YYYY - YYYY"
#'
#' @param month A numeric vector of length n representing month from 1:12
#' @param year A numreric vector of length n representing the four digit year AD
#'      , as in 2007 or 1856
#' @param split Numeric or integer representing the month your custom year
#'      starts. For example the UK crop year starts in September so you would
#'      use 9.
#'
#' @return Character vector of equal length to input vectors
#'
#' @examples
#' years <- c(2000:2010)
#' months <- c(1:12)
#'
#' yearsinput <- sample(years, 100, TRUE)
#' monthsinput <- sample(months, 100, TRUE)
#'
#' CustomYear(monthsinput, yearsinput, 9) ## for a crop year, use 4 for
#'                                        ## financial year
#' @export
CustomYear <- function(month, year, split) {
        if (length(month) != length(year)){
                stop("vectors not of equal length")
        }
        if (class(split) != "numeric") {stop("'split' must be numeric")}
        n <- length(month)
        # create empty vector of length n
        custom.year <- rep(NA, n)
        # loop records to determine crop year
        for (i in 1:paste(n)){
                prevyear <- year[i] - 1
                curryear <- year[i]
                nextyear <- year[i] + 1
                if(month[i] >= split && month[i] <= 12){
                        temp.customyear <- paste(curryear, "-", nextyear)
                }else{temp.customyear <- paste(prevyear, "-", curryear)
                }
                custom.year[i] <- temp.customyear
        }
        custom.year
}