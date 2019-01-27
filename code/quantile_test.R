# Header ####
# Script Name : Checking how percetiles work
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Tuesday, 7 March 2017  
# Version     : 1
# Description : 
#   I'm going to test here how the different percentiles functions work. I've
#   developed a funciton my self and also how the cannopy density functions
#   behave.
# License     : CC-BY-NC-SA
# File Name   : quantile_test.R

# Packages & Sources ####
library(lidR) # to be able to read the las files.

# Loading all functions in R/
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)   

# Script's Body ####

# Catalog loading ####
datapath <- file.path("data", "LiDAR Plots", "woGround")
liperi.plots.catalog <- catalog(datapath)


# Selecting a plot to use as a test plot
test.plot <- readLAS(liperi.plots.catalog[22,]) 

# Setting negative values to zero and filtering last and first returns
test.plot0 <- test.plot
test.plot0@data$Z[test.plot0@data$Z < 0] <-  0
test.plot0.f <- lasfilterfirst(test.plot0)
test.plot0.l <- lasfilterlast(test.plot0)


# Testing my function ####
h.wdt.prc(test.plot@data$Z, probs = c(.01,.2,.4,.6,.8,.95, 1))

# R's Function ####
wtd.quantile(test.plot@data$Z, weights = test.plot@data$Z, 
             probs = c(.01,.2,.4,.6,.8,.95, 1), type = "quantile")
# Density function
vegp(test.plot@data$Z, probs = c(.05,.3,.5,.7))

# Normal quantile ####
quantile(test.plot@data$Z, probs = c(.01,.2,.4,.6,.8,.95, 1))

# Shorting test plot to check values. 
sort(test.plot@data$Z)
