# Header ####
# Script Name : Function for height weighted percentiles
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Tuesday, 7 March 2017 
# Version     : 1
# Description : 
#   This is a function to calculate the weighted percentiles like Matti
#   explained to me.
# License     : CC-BY-NC-SA
# File Name   : hwtd_prc_function.R

# Packages & Sources ####

# Script's Body ####


h.wdt.prc <- function(x, probs, ...){
  sx <- sort(x)
  sumwgt <- sum(sx)
  wpost <- sumwgt * probs
  cs <- cumsum(sx) 
  perc <- c()
  for (i in 1:length(wpost)) {
    perc[i] <- which(cs >= wpost[i])[1]
  }
  result <- sx[perc]
  names(result) <- paste0(probs*100, "%")
  return(result)
}

vegp <- function(x, probs, ...){
  sx <- sort(x)
  sumwgt <- sum(sx)
  wpost <- sumwgt * probs
  cs <- cumsum(sx) 
  perc <- c()
  for (i in 1:length(wpost)) {
    perc[i] <- which(cs >= wpost[i])[1]
  }
  result <- perc / length(sx) * 100
  names(result) <- paste0(probs*100, "%")
  return(result)
}