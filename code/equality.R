# Header ####
# Script Name : Getting how equals are my metrics and Matti's ones. 
# Author/s    : Luis Puerto (lpuerto)
# Email       : luiss.puerto@gmail.com
# Date        : Tuesday, 7 March 2017  
# Version     : 1
# Description : 
#   I put together my metrics and Matti's one and I compute the mean difference
#   for them. This way I know what metrics are croocked.
# License     : CC-BY-NC-SA
# File Name   : equality.R

# Packages & Sources ####
# Ex: library(XXXX) # why? version? source? 

# Loading all functions in R/
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)   

# Script's Body ####

## let's compare some metrics and see 

plot.nu <- row.names(metrics.matti)

n.equal.m <- c("havgf", "hsdf", "hmaxf", "h5f", "h10f", "h15f", "h20f", "h25f", 
               "h30f", "h35f", "h40f", "h45f", "h50f", "h55f", "h60f", "h65f", 
               "h70f", "h75f", "h80f", "h85f", "h90f", "h95f", "veg2f", "veg5f",
               "veg10f", "veg15f", "veg20f", "hwp20f", "hwp40f", "hwp60f", 
               "hwp80f", "hwp95f", 
               
               "havgl", "hsdl", "hmaxl", "h5l", "h10l", "h15l", "h20l", "h25l", 
               "h30l", "h35l", "h40l", "h45l", "h50l", "h55l", "h60l", "h65l", 
               "h70l", "h75l", 
               "h80l", "h85l", "h90l", "h95l", "veg2l", "veg5l", "veg10l", 
               "veg15l", "veg20l", 
               "hwp20l", "hwp40l", "hwp60l", "hwp80l", "hwp95l")

n.equal.mmatti <- c("f_hmean", "f_hstd", "f_hmax", "f_h5", "f_h10", "f_h15", 
                    "f_h20", "f_h25", 
                    "f_h30", "f_h35", "f_h40", "f_h45", "f_h50", "f_h55", 
                    "f_h60", "f_h65", "f_h70",
                    "f_h75", "f_h80", "f_h85", "f_h90", "f_h95", "f_d2", "f_d5",
                    "f_d10", "f_d15",
                    "f_d20", "f_h20", "f_h40", "f_h60", "f_h80", "f_h95", 
                    
                    "l_hmean", "l_hstd", "l_hmax", "l_h5", "l_h10", "l_h15", 
                    "l_h20", "l_h25", 
                    "l_h30", "l_h35", "l_h40", "l_h45", "l_h50", "l_h55", 
                    "l_h60", "l_h65", "l_h70",
                    "l_h75", "l_h80", "l_h85", "l_h90", "l_h95", "l_d2", "l_d5",
                    "l_d10", "l_d15", 
                    "l_d20", "l_h20", "l_h40", "l_h60", "l_h80", "l_h95")

equallity2 <- all.equal(metrics[plot.nu,n.equal.m], 
                        metrics.matti[plot.nu, n.equal.mmatti])
all.equal(metrics[plot.nu,n.equal.m], 
          metrics.matti[plot.nu, n.equal.mmatti])

names(equallity2) <- c("", "", "", n.equal.m)

equality.df <- data.frame(luismetrics = n.equal.m, 
                          mattimetris = n.equal.mmatti, 
                          mean.dif = equallity2[4:67])

equality.df$mean.dif <- regmatches(equality.df$mean.dif, 
                                   regexpr("\\d[[:punct:]]\\d{1,}", 
                                           equality.df$mean.dif))

write.csv(equality.df, "output/equality.csv")
