## OPENING STATEMENTS-----------------------------------------------------------
################################################################################
## Author: John Douglass
## Date: 2014
################################################################################
## File Description;
## Purpose: A number of functions to perform  common utilities
##
## Inputs: Various
##
## Outputs: Various
##
################################################################################
## Library Statements
##
################################################################################
## FUNCTION DEFINITIONS----------------------------------------------------------

percentage.missing <- function(x, ...) {
        mean(is.na(x))
}

count.na <- function(x, ...) {
        sum(is.na(x))
}