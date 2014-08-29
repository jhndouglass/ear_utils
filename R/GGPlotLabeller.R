# From R cookbook
mf_labeller <- function(var, value){ # lifted bodily from the R Cookbook
  value <- as.character(value)
  if (var=="variable") {
    value[value=="aa"]   <- "A long label"
    value[value=="bb"]   <- "B Partners"
    value[value=="cc"]   <- "CC Inc."
    value[value=="dd"]   <- "DD Company"
    value[value=="ee"]   <- "Eeeeeek!"
    value[value=="ff"]   <- "Final"
  }
  return(value)
}