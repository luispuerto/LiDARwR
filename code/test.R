# Header ####
# Script Name : Test script
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Tuesday, 7 March 2017  
# Version     : 1
# Description : This is a script for test purposes. Most of the things here 
#               doesn't have any sense at all, so never run the whole sript
#               because it's going to yield a lot of errors. 
# License     : CC-BY-NC-SA
# File Name   : test.R

# Packages & Sources ####
# Ex: library(XXXX) # why? version? source? 

# Loading all functions in R/
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)   

# Script's Body ####




values <- c(5, 6, 7, 8, 9)
values <- seq (1,25, 1)

density.percentage <- function(x, values){
  percentage <- NA
  for (i in values){
    x.above <- x[x>i]
    percentage <- append(percentage, (length(x.above) / length(x) * 100), 
                         after = length(percentage))
  }
  return(percentage)
}


test.plot <- readLAS(liperi.plots.catalog[22,]) 

x <- sample(seq(1, 25, .01), 200, replace = T)

density.percentage(x, c(5, 6, 7))

density.percentage(lidar@data$Z, seq(1, 25, 1))
test.plot0 <- test.plot
test.plot0@data$Z[test.plot0@data$Z < 0] <-  0
test.plot0.f <- lasfilterfirst(test.plot0)
test.plot0.l <- lasfilterlast(test.plot0)

test <- (1 - ecdf(test.plot@data$Z)(seq(1, 25, 1)))*100
print(test)
test0f <- (1 - ecdf(test.plot0.f@data$Z)(seq(1, 25, 1)))*100
print(test0f)

veg.index <- function(Z, m, ...){
  for i in m {
    
  }
}
  

test.plot@data$Z[test.plot@data$Z < 0] <- 0


test.metrics <- my.plot.metrics(test.plot@data$Z)


zwtd.quat <- Hmisc::wtd.quantile(
  test.plot@data$Z, weights = test.plot@data$Z, probs = seq(0, 1, 0.05), 
  type = "(i-1)/(n-1)")
zquat <- quantile(test.plot@data$Z, probs = seq(0, 1, 0.05))


metrics.matti <- read.csv(file.path("data","ALS_leaf_off_2016_sample.txt"), 
                          sep ="\t", row.names = 1 )

test.plot.f <- lasfilterfirst(test.plot)

  
test.plot.f@data$Z
test.plot@data$Z
test.plot@data$NumberOfReturns




