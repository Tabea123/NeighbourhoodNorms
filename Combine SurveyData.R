################################################################################
##                                                                            ##
##                             Combine Survey Data                            ## 
##                                                                            ##
################################################################################

# input: CarData.csv, SolarData.csv, GreeningData.csv
#
# output: AllData.csv 

# ---------------------------------------------------------------------------- #

rm(list = ls())

library(dplyr)

Cardata <- read.csv("CarData.csv")

Cardata <- Cardata %>% 
  
  select(c(respondent, age, gender, residence, length_residency,
           n_family, n_friends, n_acquaintances,
           B, PNB, NE, EE, NE_cent, EE_cent, PNB_cent,
           conversations))

Solardata <- read.csv("SolarData.csv")

Solardata <- Solardata %>% 
  
  select(c(respondent, age, gender, residence, length_residency,
           n_family, n_friends, n_acquaintances,
           B, PNB, NE, EE, NE_cent, EE_cent, PNB_cent,
           conversations))

Greeningdata <- read.csv("GreeningData.csv")

Greeningdata <- Greeningdata %>% 
  
  select(c(respondent, age, gender, residence, length_residency,
           n_family, n_friends, n_acquaintances,
           B, PNB, NE, EE, NE_cent, EE_cent, PNB_cent,
           conversations))

alldata <- rbind(Solardata, Cardata, Greeningdata) 