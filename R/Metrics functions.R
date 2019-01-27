# Header ####
# Script Name : Functions for metrics
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Thursday, 16th February 2017
# Version     : 1
# Description : 
#   We are going to define some functions to yield the metrics for the later
#   analysis of the plots
# License     : CC-BY-NC-SA
# File Name   : Metrics functions.R

# Packages & Sources ####
library(lidR) # to be able to read LiDAR data and use other functions
library(Hmisc)

 # Script's Body ####

# Function with all the metrics we are going to calculate for each plot. 
# This is a function aiming to anaylice height data of each plot.

my.plot.metrics <- function(x, ...) {
  # we use it to calculate metrics
  metrics <- c(
    mean <- mean(x), # The mean of all the height.             
    standar.deviation <- sd(x), # The standar deviation of the heights.
    maximun.val <- max(x), # The maximun value of the heights.
    # Quantiles for the heigths with a 5% pace.
    height.quantiles <- quantile(x, probs = seq(0, 1, 0.05)),
    # Percentage of points above a given value from 1 to 25 with 1m pace.
    density.percentaje <- (1 - ecdf(x)(seq(1, 25, 1)))*100,
    # Height weighted quantiles 
    height.wgd.quant <- wtd.quantile(
      x, weights = x, probs = c(.01,.2,.4,.6,.8,.95, 1), type = "quantile"),
    # Calculating the % of point abouve specific percentiles
    canopy.density <- vegp(x, probs = c(.05,.3,.5,.7)))
  # naming the output
  names(metrics) <- 
    c("avg", "sd", "max", paste0("p", seq(0, 100, 5), "%"), 
      paste0("d", seq(1, 25, 1), "m"), 
      paste0("wp", c("1%","20%","40%","60%","80%","95%","100%")), 
      paste0("dp", c("5%", "30%", "50%", "70%")))
  return(metrics)
}

# Function to apply to all the plots. This will create a data frame 
# with all the data the names. It will also filter the points and will set
# them to zero when they are negative.  

my.metrics.FirsLast <- function(LASFile, ...) {
  lidar <- readLAS(LASFile) # reading a LAS file
  lidar@data$Z[lidar@data$Z < 0] <- 0 # setting to zero negative returns
  lidar.f <- lasfilterfirst(lidar) # Filter first of many returns
  lidar.l <- lasfilterlast(lidar) # Filter last of many 
  # Calculating the metrics for the first of many returns. 
  metrics.f <- lasmetrics(lidar.f, my.plot.metrics(Z))
  # Labeling the output
  names(metrics.f) <- c("havgf",  "hsdf", "hmaxf",
                        paste0("h",seq(0, 100, 5), "f"), 
                        paste0("veg", seq(1, 25, 1), "f"),
                        paste0("hwp", 
                              c("1","20","40","60","80","95","100"),"f"), 
                        paste0("vegp", c("5", "30", "50", "70"), "f"))
  # Calculating the metrics for the first of many returts. 
  metrics.l <- lasmetrics(lidar.l, my.plot.metrics(Z))
  # Labeling the output
  names(metrics.l) <- c("havgl",  "hsdl", "hmaxl", 
                        paste0("h",seq(0,100,5),"l"), 
                        paste0("veg", seq(1, 25, 1), "l"),
                        paste0("hwp", 
                              c("1","20","40","60","80","95","100"),"l"), 
                        paste0("vegp", c("5", "30", "50", "70"), "l"))
  
  metrics <- c(metrics.f, metrics.l) # combining first and last in one output
  return(metrics)
}

