# Header ####
# Script Name : Model for Liperi Data
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Thursday, 17 March 2017  
# Version     : 1
# Description : This is a template for R scripts. For the general ones.
# License     : CC-BY-NC-SA
# File Name   : liperi modeling.R

# Packages & Sources ####
# Ex: library(XXXX) # why? version? source? 
library(readxl)
# Loading all functions in R/
# sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)   

# Script's Body ####

# Load the metrics ####

load("var/metrics.RData")

# Load the biomass data ####

liperi.bio <- read_excel("data/Liperi_biomass.xlsx", sheet = "plot_data_V_AGB")

# Cleaning the df

liperi.bio <- liperi.bio[, -c(1, 3)]
colnames(liperi.bio) <-  c("ID", "type", "r", "x", "y", "add", "Vol", "AGB")

# Seting "type" as a

liperi.bio$type <-  as.factor(liperi.bio$type)

# save as variable to later lazy loading

save(liperi.bio, file = "var/liperi.bio.RData")

# Join the data

metrics.bio <- merge(liperi.bio, metrics, by = intersect("ID", "ID"))

# Create the model ####

# geting the names for the formula

ind.var <- colnames(metrics)
ind.var <- ind.var[-1]
ind.var <- paste(ind.var, collapse = " + ", sep = "")
formula <- paste0("AGB ~ ", ind.var)

# Creating the initial model with all the variables 
init.model <- lm(formula, data = metrics.bio)
summary(init.model)

### Select a set of independent variables by stepwise AIC. The number of
### variables is controlled by the value of k parameter.
step.model <- step(init.model, direction= "backward",
                            trace=0, k=13)
summary(step.model)

### Plot residuals vs. fitted
plot(fitted(step.model), residuals(step.model))
abline(0, 0, col="red" )

# Transform AGB to log ####

metrics.bio$AGBlog <- log(metrics.bio$AGB)

# New formula

formula.log <- paste0("AGBlog ~ ", ind.var)

# Creating the initial model with all the variables 
init.model.agb.log <- lm(formula.log, data = metrics.bio)
summary(init.model)

### Select a set of independent variables by stepwise AIC. The number of
### variables is controlled by the value of k parameter.
step.model.agb.log <- step(init.model.agb.log, direction= "backward",
                            trace=0, k=13)
summary(step.model.agb.log)

### Plot residuals vs. fitted
plot(fitted(step.model.agb.log), residuals(step.model.agb.log))
abline(0, 0, col="red" )

# Transform AGB to sqrt ####

metrics.bio$AGBsqrt <- sqrt(metrics.bio$AGB)

# New formula

formula.sqrt <- paste0("AGBsqrt ~ ", ind.var)

# Creating the initial model with all the variables 
init.model.agb.sqrt <- lm(formula.sqrt, data = metrics.bio)
summary(init.model)

### Select a set of independent variables by stepwise AIC. The number of
### variables is controlled by the value of k parameter.
step.model.agb.sqrt <- step(init.model.agb.sqrt, direction= "backward",
                            trace=0, k=13)
summary(step.model.agb.sqrt)

### Plot residuals vs. fitted
plot(fitted(step.model.agb.sqrt), residuals(step.model.agb.sqrt))
abline(0, 0, col="red" )

