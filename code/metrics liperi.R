# Header ####
# Script Name : Analysis of plots in Liperi
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Thursday, 16 February 2017 
# Version     : 1
# Description : 
#   We are going to get all the necesary metrics from the plots in Liperi. 
#   We've clipped and normalized the plots previously in Fusion. 
#   To normalize the plots we've used the DTM from Finnish National Survery. 
# License     : CC-BY-NC-SA
# File Name   : analysis liperi 20170216.R

# Packages & Sources ####
library(lidR)
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)

# Script's Body ####
# Metrics computing ####
# Load the data to a catalog
datapath <- file.path("data", "LiDAR Plots")
liperi.plots.catalog <- catalog(datapath)

# Now we can plot the catalog, if we want. 

plot(liperi.plots.catalog)

# We apply calculate the metrics in all the plots

metrics.matrix <- catalog_apply(
  liperi.plots.catalog, my.metrics.FirsLast, mc.cores = 1)

# Transform the output to a from a matrix to a data frame

metrics <- as.data.frame(metrics.matrix)

# We add the plot number to each metric 

metrics$ID <- as.numeric(
  substr(liperi.plots.catalog$filename, 27, 
         nchar(liperi.plots.catalog$filename)-4))

metrics <- metrics[,c(121,1:120)]
row.names(metrics) <- metrics$ID

# Write a csv with all the metrics

write.csv2(metrics, "output/metrics.csv")

# Kotivuori 2016 ####
# Calculate the volumen and biomass with the Kotivuori 2016 general models

## Loading the predictors for the functions 
havgf <- metrics$havgf  # Last returns height average
h95l <- metrics$h95l  # Last returns percentile 95%
hmaxl <- metrics$hmaxl  # Last returns maximum height

## Calcualting with the models
# Volumen 
# sqrt(V) = 0.7622 + 3.3582 × sqrt(havgF) + 0.0100 × h95L^2
vol.gen <- (.7622+3.3582*sqrt(havgf)+.0100*h95l^2)^2
# Biomass 
# sqrt(Mt) = –0.4247 + 0.1494 × hmaxL + 2.5196 × sqrt(havgf)
bio.gen <- (-.4247+.1494*hmaxl+2.5196*sqrt(havgf))^2

# Calculating the local Kotivuori 2016 (Sulkava) models for volume and biomass 

## Loading the predictors
h70f <- metrics$h70f  # First returns percentile 70% 
veg9l <- metrics$veg9l  # Last returns vegetation den. index 9m above 
veg8l <- metrics$veg8l  # Last returns vegetation den. index 8m above

## Calculating with the models 
# Volumen
# sqrt(V) = 3.4744 + 0.5599 × h70F + 0.0894 × veg9L
vol.sulkava <- (3.4744+.5599*h70f+.0894*veg9l)^2
# biomass 
# sqrt(Mt) = 2.9455 + 0.3557 × h70F + 0.0702 × veg8L
bio.sulkava <- (2.9455+.03557*h70f+.0702*veg8l)^2 

## Create a dataframe with all the Kotivuori 2016 models. 
all.models.kotivuori <- data.frame(
  ID=metrics$ID, vol.gen, bio.gen, vol.sulkava, bio.sulkava)

## write the outcome in a table 
write.csv(all.models.kotivuori, "output/kotivuori.models.csv")

## Residual Corrections
vol.gen.re <- vol.gen + 2.0923
bio.gen.re <- bio.gen + 1.1161
vol.sul.re <- vol.sulkava + 3.2302
bio.sul.re <- bio.sulkava + 1.6063

## Create a dataframe w/ results 
all.mod.koti.re <- data.frame(
  ID=metrics$ID, vol.gen.re, bio.gen.re, vol.sul.re, bio.sul.re)

## Write to a csv
write.csv(all.models.kotivuori, "output/kotivuori.models.re.csv")

# Villikka 2011 ####
# Calculate the volume & biomass w/ Villikka 2011 models 

## Loading the predictors

hsdl <- metrics$hsdl
havgf <- metrics$havgf
vegp50l <- metrics$vegp50l 
havgl <- metrics$havgl
h80l <- metrics$hwp80l

## Leaf off models

leaf.off.V <- (.19142 + .72751* hsdl + 3.34859 * sqrt(havgf))^2
leaf.off.B <- exp(5.44349 - .03312* vegp50l + .70593 * sqrt(havgf))

## Leaf on models

leaf.on.V <- exp(3.600282 + .154488 * havgl + .045838 * h80l)
leaf.on.B <- (3.15443 + 0.6478 * hsdl + 1.89642 * log(havgl))^2

## Create a dataframe with all the Villikka 2011 models. 

all.models.villikka <- data.frame(
  ID=metrics$ID, leaf.off.V, leaf.off.B, leaf.on.V, leaf.on.B)

## Write it in a csv

write.csv(all.models.villikka, "output/villikka.models.csv")


# Saving results ####

results.all.models <- c("all.mod.koti.re", 
                        "all.models.kotivuori", 
                        "all.models.villikka")
save(list = results.all.models, file = "var/results.all.models.RData")
save(metrics, file = "var/metrics.RData")
