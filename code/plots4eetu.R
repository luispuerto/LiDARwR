# Header ####
# Script Name : Plots for Eetu
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Thursday, 2 March 2017 
# Version     : 1
# Description : 
#   This is how I got the random plots fot the calculations Eetu carried out. 
#   There is also here the way to classify the returns in UEF way.
# License     : CC-BY-NC-SA
# File Name   : plots4eetu.R

# Packages & Sources ####
library(lidR) # to play witht the LiDAR data d

sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)

# Script's Body ####

# Selection ####
# Selection of plots randomly
set.seed(58945804) 
randomplots <- floor(runif(5, 1, 575))
randomplots

# Creation of a list to store each of the LiDAR objects
myplots.list <- vector("list",length(randomplots))

# Reading the plots from the catalog
for (i in 1:length(randomplots)){
  myplots.list[i] <- readLAS(liperi.plots.catalog[randomplots[i],])
}

# Creation of a variable in each of the plot to indicate the number of the plot 
# for each return. 
for (i in 1:5) {
  myplots.list[[i]]@data$plot_id <- as.numeric(
    regmatches(liperi.plots.catalog[randomplots[i], "filename"],
               regexpr("\\d+", 
                       liperi.plots.catalog[randomplots[i], "filename"])))  
}

# aggregrate all the ploints to a single data frame 
rplots.returns.df <- data.frame()
for (i in 1:length(myplots.list)){
  rplots.returns.df <- rbind(rplots.returns.df, myplots.list[[i]]@data)
}

# UEF Classification ####
rplots.returns.df$echotype[rplots.returns.df$NumberOfReturns==1] <- 0
rplots.returns.df$echotype[(rplots.returns.df$NumberOfReturns>1 & 
                              rplots.returns.df$ReturnNumber==1)] <- 1 
rplots.returns.df$echotype[(rplots.returns.df$NumberOfReturns>2 &  
                              (rplots.returns.df$ReturnNumber<
                                 rplots.returns.df$NumberOfReturns) &
                              rplots.returns.df$ReturnNumber!=1)] <- 2 
rplots.returns.df$echotype[(rplots.returns.df$NumberOfReturns>1 &  
                              (rplots.returns.df$ReturnNumber==
                                 rplots.returns.df$NumberOfReturns))] <- 3

r4eetu <- with(rplots.returns.df, data.frame(x=X, y=Y, dz=Z, echotype, plot_id))

# Writing the file ####
write.table(r4eetu, "output/r4eetu.txt", sep = "\t", row.names = F)


# checking some values

r4eetu[r4eetu$dz>30,]

randomplotsid <- c()
for (i in 1:5) {
  randomplotsid[i] <- as.numeric(
    regmatches(liperi.plots.catalog[randomplots[i], "filename"],
               regexpr("\\d+", 
                       liperi.plots.catalog[randomplots[i], "filename"])))  
}
print(randomplotsid)


# getting my metrics for the same plots to make the comparison. 
write.csv(metrics[randomplots, ], "output/metrics random.csv")
View(metrics[randomplots,])







