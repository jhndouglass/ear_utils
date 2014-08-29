dnrm <- function(vars, envir = .GlobalEnv) { 
  vars <- c(vars, "dnrm") 
  keep <- match(x = vars, table = ls(envir = envir)) 
  if(any(is.na(keep))) { 
    stop(paste("Some of the variables were not found in", 
               environmentName(envir))) 
  } 
  rm(list = ls(envir = envir)[-keep], envir = envir) 
  cat("Removed all but", length(keep), "objects from", 
      environmentName(envir), fill = TRUE) 
} 