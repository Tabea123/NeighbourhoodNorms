################################################################################
##                                                                            ##
##                       Data Processing Greening                             ##
##                                                                            ##
################################################################################

# input: questionnaire-9937_2021-04-26_09.17.00.csv
#
# output: 

#-------------------------------------------------------------------------------
#                                                               
#                     #### 1. Load Packages, Load Data ####
#                                                               
#-------------------------------------------------------------------------------

# setwd("~/Groningen/Project 1/NeighbourhoodNorms/Greening/")

rm(list = ls())

# LOAD PACKAGES #
library(dplyr)
library(mice)
library(psych)
library(ggplot2)
library(reshape)

# DATA CLEANING #
data <- read.csv2("questionnaire-9937_2021-04-26_09.17.00.csv") # greening

## select columns
data2 <- data %>% dplyr::select(c(1, 11, 17:19, 25, 31, 37, 43, 49, 55, 61, 67, 73, 
                                  79, 85, 91, 97, 104, 111, 118, 125, 132, 139, 146, 
                                  151:153, 159, 166, 173, 180))
## rename columns
colnames(data2)

colnames(data2)[1:5] <- c("respondent", "residence", "n_family", 
                          "n_friends", "n_acquaintances")

colnames <- gsub(pattern = "Geef.aan.in.hoeverre.de.volgende.stellingen.kloppen.._.", 
                 replacement = "B.", x = colnames(data2), fixed = T)
colnames <- gsub(pattern = "Geef.aan.hoeveel.mensen.in.deze.buurt.meedoen.in.de.volgende.activiteiten.._.", 
                 replacement = "EE.", x = colnames, fixed = T)
colnames <- gsub(pattern = "Geef.aan.in.hoeverre.u.het.eens.bent.met.de.volgende.stellingen.._.Ik.verwacht", 
                 replacement = "NE.Ik.verwacht", x = colnames, fixed = T)
colnames <- gsub(pattern = "Geef.aan.in.hoeverre.u.het.eens.bent.met.de.volgende.stellingen.._.", 
                 replacement = "PNB.", x = colnames, fixed = T)
colnames <- gsub(pattern = ".", replacement = " ", x = colnames, fixed = T)

colnames[26:29] <- c("gender", "age", "length_residency", "conversations")
colnames[30:32] <- paste0(rep("attachment", 3), 1:3)

colnames(data2) <- colnames


## make answers numerical
data3 <- data2 %>% mutate(across(starts_with(c("PNB")), 
                                 ~as.numeric(factor(., levels = c("Sterk mee oneens", 
                                                                  "Mee oneens", "Neutraal", "Mee eens", "Sterk mee eens")))))

data3 <- data3 %>% mutate(across(starts_with(c("NE")), 
                                 ~as.numeric(factor(., levels = c("Sterk mee oneens", 
                                                                  "Mee oneens", "Neutraal", "Mee eens", "Sterk mee eens")))))

data3 <- data3 %>% mutate(across(starts_with("EE"), 
                                 ~as.numeric(factor(., levels = c("Niemand", "Enkele", 
                                                                  "Een flink aantal", "Bijna iedereen", "Iedereen")))))

data3 <- data3 %>% mutate(across(starts_with("B"), 
                                 ~as.numeric(factor(., levels = c("Geheel onjuist voor mij", 
                                                                  "Onjuist voor mij", "Een beetje onjuist voor mij",
                                                                  "Een beetje juist voor mij", "Juist voor mij", 
                                                                  "Geheel juist voor mij")))))

data3 <- data3 %>% mutate(across(starts_with("attachment"), 
                                 ~as.numeric(factor(., levels = c("Geheel onjuist voor mij",
                                                                  "Een beetje juist voor mij", "Juist voor mij", 
                                                                  "Geheel juist voor mij")))))

data3 <- data3 %>% mutate(across(starts_with("conversations"), 
                                 ~as.numeric(factor(., levels = c("Nooit", "Zelden", 
                                                                  "Soms", "Vaak")))))

## reverse scaling
colnames(data3)
### Ik heb tegels in mijn tuin gelegd om te voorkomen dat ik een tuin moest onderhouden
data3[,19] <- 7 - data3[,19]
### Ik heb nooit overwogen om meer groen rondom mijn huis aan te leggen
data3[,20] <- 7 - data3[,20]

## add information for duplicate neighbourhood
data3$residence <- sub("Oosterpoort", "Oosterpoort (greening)", data3$residence)


nrow(data3) # 119 respondents


#-------------------------------------------------------------------------------
#                                                               
#               #### 3. Exclude Participants, Impute Data ####
#                                                               
#-------------------------------------------------------------------------------

# EXCLUDE PARTICIPANTS #

# listwise deletion
listwise <- na.omit(data3)
nrow(listwise) # 74 respondents
(1 - nrow(listwise)/nrow(data3)) * 100 
# ~ 38% of the data would be discarded with listwise deletion

# alternative
## first exclude all respondents that did not answer anything to either
## NE, EE or B

count_na <- function(x) sum(is.na(x))

data4 <- data3 %>% rowwise %>% 
  
  mutate(na_PNB = count_na(c_across(starts_with("PNB"))),
         na_B = count_na(c_across(starts_with("B"))), 
         na_EE = count_na(c_across(starts_with("EE"))),
         na_NE = count_na(c_across(starts_with("NE")))) %>%
  
  filter(na_PNB < 4, na_B < 4, na_EE < 4, na_NE < 4) %>%
  
  select(-c(na_PNB, na_B, na_EE, na_NE))


## check for extreme values for neighbourhood residency and number of contacts
max(data4$age, na.rm = T)
max(data4$length_residency, na.rm = T) # months
max(data4$n_family, na.rm = T)
max(data4$n_friends, na.rm = T)
max(data4$n_acquaintances, na.rm = T)

nrow(data4) # 106 respondents
(1 - nrow(data4)/nrow(data3)) * 100 # ~ 11% of the data is discarded


# DATA IMPUTATION # 

## select + rename relevant columns 
subset_data <- data4 %>% select(contains(c("PNB", "B", "NE", "EE")))
data5 <- rename_with(subset_data, ~ paste0("B", 1:7), starts_with("B")) 
data5 <- rename_with(data5, ~ paste0("PNB", 1:4), starts_with("PNB")) 
data5 <- rename_with(data5, ~ paste0("EE", 1:4), starts_with("EE")) 
data5 <- rename_with(data5, ~ paste0("NE", 1:5), starts_with("NE"))


## impute missing data
imp <- mice(data5, m = 5) # m = 1 results in a single imputation
data6 <- complete(imp)

#-------------------------------------------------------------------------------
#                                                               
#           #### 4. Compute Mean Scores, Center Variables ####
#                                                               
#-------------------------------------------------------------------------------

data7 <- data6 %>% rowwise() %>% mutate(B = mean(c_across(starts_with("B"))))
data7 <- data7 %>% rowwise() %>% mutate(PNB = mean(c_across(starts_with("PNB"))))
data7 <- data7 %>% rowwise() %>% mutate(NE = mean(c_across(starts_with("NE"))))
data7 <- data7 %>% rowwise() %>% mutate(EE = mean(c_across(starts_with("EE"))))

## add respondent number, residence and contacs again
data8 <- bind_cols(tibble(data7), 
                   tibble(respondent       = data4$respondent),
                   tibble(age              = data4$age),
                   tibble(gender           = data4$gender),
                   tibble(conversations    = data4$conversations),
                   tibble(residence        = data4$residence),
                   tibble(length_residency = data4$length_residency),
                   tibble(n_family         = data4$n_family),
                   tibble(n_friends        = data4$n_friends),
                   tibble(n_acquaintances  = data4$n_acquaintances)) 
## mean center
data8 <- data8 %>% mutate(NE_cent = NE-mean(NE), EE_cent = EE-mean(EE), PNB_cent = PNB-mean(PNB))

#-------------------------------------------------------------------------------
#                                                               
#                           #### 5. Export Data ####
#                                                               
#-------------------------------------------------------------------------------

write.csv(data8, "GreeningData.csv")

#-------------------------------------------------------------------------------
#                                                               
#                            #### 6. Plot Data ####
#                                                               
#-------------------------------------------------------------------------------

# Behaviour
B_dataframe <- data8 %>% select(B) %>% melt(variable = "Greening")

png("Greening_Raincloud_B.png")
ggplot(B_dataframe, aes(x = Greening, y = value, ylim(1, 6))) + 
  
  ## data points
  geom_point(
    
    color = "seagreen4",
    ## draw bigger points
    size = 1.5,
    ## add some transparency
    alpha = .3,
    ## add some jittering
    position = position_jitter(seed = 1, width = .2)
    
  ) +
  
  ## half-violin 
  ggdist::stat_halfeye(
    
    ## adjust color
    slab_fill   = "seagreen4",
    slab_alpha  = 0.5, 
    ## bandwidth (how smooth is the distribution?)
    adjust = .6, 
    ## move to the right
    justification = -.3,
    ## remove slab interval
    .width = 0,
    point_colour = NA
    
  ) + 
  
  ## boxplot
  geom_boxplot(
    
    colour = "seagreen4",
    width = .12,
    ## do not plot outliers
    outlier.color = NA,
    ## change position
    position = position_nudge(x = -0.35)
    
  ) +
  
  ## remove white space 
  coord_cartesian(xlim = c(1.7, 0.5), ylim = c(1, 6)) +
  
  ## remove background
  theme_minimal()

dev.off()


# Personal Normative Beliefs
PNB_dataframe <- data8 %>% select(PNB) %>% melt(variable = "Greening")

png("Greening_Raincloud_PNB.png")
ggplot(PNB_dataframe, aes(x = Greening, y = value)) + 
  
  ## data points
  geom_point(
    
    color = "palegreen4",
    ## draw bigger points
    size = 1.5,
    ## add some transparency
    alpha = .3,
    ## add some jittering
    position = position_jitter(seed = 1, width = .2)
    
  ) +
  
  ## half-violin 
  ggdist::stat_halfeye(
    
    ## adjust color
    slab_fill   = "palegreen4",
    slab_alpha  = 0.5, 
    ## bandwidth (how smooth is the distribution?)
    adjust = .6, 
    ## move to the right
    justification = -.3,
    ## remove slab interval
    .width = 0,
    point_colour = NA
    
  ) + 
  
  ## boxplot
  geom_boxplot(
    
    colour = "palegreen4",
    width = .12,
    ## do not plot outliers
    outlier.color = NA,
    ## change position
    position = position_nudge(x = -0.35)
    
  ) +
  
  ## remove white space 
  coord_cartesian(xlim = c(1.7, 0.5), ylim = c(1, 5)) +
  
  ## remove background
  theme_minimal()

dev.off()


# Normative Expectations
NE_dataframe <- data8 %>% select(NE) %>% melt(variable = "Greening")

png("Greening_Raincloud_NE.png")
ggplot(NE_dataframe, aes(x = Greening, y = value, ylim(1, 6))) + 
  
  ## data points
  geom_point(
    
    color = "green4",
    ## draw bigger points
    size = 1.5,
    ## add some transparency
    alpha = .3,
    ## add some jittering
    position = position_jitter(seed = 1, width = .2)
    
  ) +
  
  ## half-violin 
  ggdist::stat_halfeye(
    
    ## adjust color
    slab_fill   = "green4",
    slab_alpha  = 0.5, 
    ## bandwidth (how smooth is the distribution?)
    adjust = .6, 
    ## move to the right
    justification = -.3,
    ## remove slab interval
    .width = 0,
    point_colour = NA
    
  ) + 
  
  ## boxplot
  geom_boxplot(
    
    colour = "green4",
    width = .12,
    ## do not plot outliers
    outlier.color = NA,
    ## change position
    position = position_nudge(x = -0.35)
    
  ) +
  
  ## remove white space 
  coord_cartesian(xlim = c(1.7, 0.5), ylim = c(1, 5)) +
  
  ## remove background
  theme_minimal()

dev.off()


#-------------------------------------------------------------------------------
#                                                               
#                   #### 7. Calculate Scale Mean, SD, Alpha ####
#                                                               
#-------------------------------------------------------------------------------

# PNB
round(mean(data8$PNB),3)
round(sd(data8$PNB),3)
PNB.items <- data8 %>% select(PNB1:PNB4) 
round(psych::alpha(PNB.items)$total$std.alpha,3) # 0.87

# NE
round(mean(data8$NE),3)
round(sd(data8$NE),3)
NE.items <- data8 %>% select(NE1:NE5) 
round(psych::alpha(NE.items)$total$std.alpha,3) # 0.72

# EE
round(mean(data8$EE),3)
round(sd(data8$EE),3)
EE.items <- data8 %>% select(EE1:EE4) 
round(psych::alpha(EE.items)$total$std.alpha,3) # 0.63

# B
round(mean(data8$B),3)
round(sd(data8$B),3)
B.items <- data8 %>% select(B1:B7) 
round(psych::alpha(B.items)$total$std.alpha,3) # 0.7




