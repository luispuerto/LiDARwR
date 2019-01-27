# Header ####
# Script Name : Calculating the RMSE and RMSE%
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Monday, 13 March 2017  
# Version     : 1
# Description : 
#   This is a  script to calculate the RMSE and RMSE% for the for the models
#   I've been calculating.
# License     : CC-BY-NC-SA
# File Name   : RMSE cal.R

# Packages & Sources ####
# Ex: library(XXXX) # why? version? source? 
library(readxl)  # I'm gonna read data from a xlsx file
library(xlsx)  # I'm going to read and write data

# Loading all functions in R/
# * I don't need this for this script
# sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)   

# Script's Body #### 

# Reading the original data from the xlsx with the biomass data. 
liperi.bio <- read_excel("data/Liperi_biomass.xlsx", sheet = "plot_data_V_AGB")

# Loading the previous data 

load("var/results.all.models.RData")

# Merge kotivuori and observed biomass

kotivuori.merged.bio <- merge(all.mod.koti.re, liperi.bio, 
                              by.x = "ID", by.y = "plot_id")

# Merge all models biomass

all.models.agb <- merge(kotivuori.merged.bio, all.models.villikka, 
                        by.x = "ID", by.y = "ID")

rm(kotivuori.merged.bio, all.models.kotivuori)

# Create a clean df. 

all.models.agb <- all.models.agb[,c(1, 3, 5, 16, 18, 14)]

colnames(all.models.agb) <- c("ID", "KV.AGB.g", 
                              "KV.AGB.s", "VI.off", "VI.on", "AGB.obs")

# RMSE Calculation

KV.RSME <- sqrt(mean((all.models.agb$AGB.obs - 
                        all.models.agb$KV.AGB.g)^2))
KV.RSME.nor <- (KV.RSME / mean(all.models.agb$AGB.ob))*100

KV.RSME.su <- sqrt(mean((all.models.agb$AGB.obs - 
                           all.models.agb$KV.AGB.s)^2))
KV.RSME.nor.su <- (KV.RSME.su / mean(all.models.agb$AGB.obs))*100

VI.RMSE.off <- sqrt(mean((all.models.agb$AGB.obs - 
                            all.models.agb$VI.off)^2))
VI.RMSE.off.nor <- (VI.RMSE.off / mean(all.models.agb$AGB.obs))*100

VI.RMSE.on <- sqrt(mean((all.models.agb$AGB.obs - 
                            all.models.agb$VI.on)^2))
VI.RMSE.on.nor <- (VI.RMSE.on / mean(all.models.agb$AGB.obs))*100

# Create a df with the resutls

Values <- c(KV.RSME, KV.RSME.nor, KV.RSME.su, 
            KV.RSME.nor.su, VI.RMSE.off, VI.RMSE.off.nor, 
            VI.RMSE.on, VI.RMSE.on.nor)
names(Values) <- c("KV.RSME", "KV.RSME.nor", "KV.RSME.su", 
                   "KV.RSME.nor.su", "VI.RMSE.off", "VI.RMSE.off.nor", 
                   "VI.RMSE.on", "VI.RMSE.on.nor")
RMSEs <- data.frame(values = round(Values, digits = 2), row.names = names(Values))

# Write as a csv
write.xlsx(RMSEs, file = "output/rmses.xlsx")





