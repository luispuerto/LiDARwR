# Header ----
# Script Name : Reading LiDAR Plots for Analysis 
# Author/s    : Luis Puerto
# Username/s  : lpuerto
# Email       : luiss.puerto@gmail.com 
# Date        : Wednesday, February 8th, 2017. 
# Version     : 1
# Updated     : NULL
# Description : 
#   This is a script to read and analyze LiDAR plots from Liperi area
# System      : Mac OS X 10.11.6 / Windows 10
# R version   : R 3.3.2 (64 Bit) (2016-10-31) "Sincere Pumpkin Patch"
# License     : CC-BY-NC-SA
# File Name   : read_plots.R 

# Packages to load ----
# Ex: library(XXXX) # version 1.2.3 from CRAN or source
library(lidR)

# Script's Body ----

# First we load the data we are going to play with. 
# We are going to use the plots that have been normalized with dtm from 
# national Finnish survey in Fusion

liperi.plots.catalog <- catalog(file.path("data", "LiDAR Plots", "woGround"))

# Now we plot the catalog to see the plots

plot(liperi.plots.catalog)

# definition of the metrics we want to get

plot.metrics <- function(x) {
  metrics <- c(
    mean <- mean(x),
    standar.deviation <- sd(x), 
    maximun.val <- max(x), 
    height.quantiles <- quantile(x, probs = seq(0, 1, 0.05)),
    density.percentaje <- (1 - ecdf(x)(seq(1, 25, 1)))*100
  )
  return(metrics)
}

lidar <- readLAS(liperi.plots.catalog[123,], all = T)
lidar %>% lasmetrics(.stdmetrics)

mymetrics <- function(LASFile) {
  lidar <- readLAS(LASFile)
  lidar@data$Z[lidar@data$Z < 0] <- 0
  lidar.f <- lasfilterfirstofmany(lidar)
  lidar.l <- lasfilterlast(lidar)
  
  metrics.f <- lasmetrics(lidar.f, plot.metrics(Z))
  names(metrics.f) <- c("havef",  "hsdf", "hmaxf", paste0("h", seq(0,100, 5), "f"), 
                        paste0("veg", seq(1, 25, 1), "f"))
  
  metrics.l <- lasmetrics(lidar.l, plot.metrics(Z))
  names(metrics.l) <- c("havgl",  "hsdl", "hmaxl", paste0("h", seq(0,100, 5), "l"), 
                        paste0("veg", seq(1, 25, 1), "l"))
  
  metrics <- c(metrics.f, metrics.l)
  
  return(metrics)
}

# This is not needed right now because I am using just one core 
# export <- c("metrics", "lidar", "LASFile", "readLAS", "lasmetrics")
# plot.quantiles <- catalog_apply(liperi.plots.catalog, mymetrics, 
#                                varlist = export, platform = "windows")

plot.quantiles <- catalog_apply(liperi.plots.catalog, mymetrics, 
                                mc.cores = 1)
     plot.metrics <- as.data.frame(plot.quantiles)

plot.metrics$ID <- substr(liperi.plots.catalog$filename, 27, 
                          nchar(liperi.plots.catalog$filename)-4)

liperi.metrics <- plot.metrics[,c(49,1:48)]


# lasmetrics(lasfilter(single_plot, Z >= 0), quantile)
# single_plot.zeros <- lasfilter(single_plot, Z <= 0)  
# single_plot.zeros <- single_plot
# single_plot.zeros@data$Z [single_plot.zeros@data$Z < 0 ]  <- 0
test.read.las <- readlasdata(liperi.plots.catalog$filename[123])
