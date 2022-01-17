################################################################################
##                                                                            ##
##                        Analysis of Combined Data Set                       ## 
##                                                                            ##
################################################################################


# input: AllData.csv                                                           
#                                                                              
# output:                                                                                                                                                  #

#-------------------------------------------------------------------------------
#                                                               
#                     #### 1. Load Packages, Load Data ####
#                                                               
#-------------------------------------------------------------------------------

rm(list = ls())

# load packages
library(performance)
library(ggplot2)
library(dplyr)
library(reshape)

# load data
alldata <- read.csv("AllData.csv")

#-------------------------------------------------------------------------------
#                                                               
#                     #### 2. Analyse Demographics ####
#                                                               
#-------------------------------------------------------------------------------

# final number of participants
nrow(alldata) # 258

# gender
summary(alldata$gender)

# age
alldata %>% summarise(mean = mean(age, na.rm = T), sd = sd(age, na.rm = T),
                      na = sum(is.na(age)))

# length of residency
alldata %>% 
  # calculate mean and sd
  summarise(mean = mean(length_residency, na.rm = T), 
            sd   = sd(length_residency, na.rm = T),
            na = sum(is.na(length_residency)))


# number of contacts
summary(alldata$n_family)
summary(alldata$n_friends)
summary(alldata$n_acquaintances)


#-------------------------------------------------------------------------------
#                                                               
#                 #### 3. Pluralistic Ignorance Analysis ####
#                                                               
#-------------------------------------------------------------------------------

# Data Distribution
subset1 <- alldata %>% select(PNB, NE) %>% melt()
subset2 <- alldata %>% select(B, EE) %>% melt()
subset3 <- alldata %>% select(B) %>% melt()
subset4 <- alldata %>% select(PNB) %>% melt()

sample_size1 <- subset1 %>% group_by(variable) %>% summarize(num=n())
sample_size2 <- subset2 %>% group_by(variable) %>% summarize(num=n())
sample_size3 <- subset3 %>% group_by(variable) %>% summarize(num=n())
sample_size4 <- subset4 %>% group_by(variable) %>% summarize(num=n())


subset1 %>%
  left_join(sample_size1) %>% 
  mutate(myaxis = paste0(variable, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = value, fill = variable)) +
  geom_violin(width = 1, alpha = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Answers", limits = c(1,5)) +
  scale_fill_manual(values=c("#F8766D", "#72B000")) +
  theme_minimal()



subset2 %>%
  left_join(sample_size2) %>% 
  mutate(myaxis = paste0(variable, "\n", "n=", num)) %>%
  ggplot( aes(x = myaxis, y = value, fill = variable)) +
  geom_violin(width = 1, alpha = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name ="Answers", limits = c(1,6)) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme_minimal()


subset3 %>%
  left_join(sample_size3) %>% 
  mutate(myaxis = paste0(variable, "\n", "n=", num)) %>%
  ggplot( aes(x = myaxis, y = value, fill = variable)) +
  geom_violin(width = 0.7, alpha = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name ="Answers", limits = c(1,6)) +
  scale_fill_manual(values=c("#E69F00")) +
  theme_minimal()


subset4 %>%
  left_join(sample_size4) %>% 
  mutate(myaxis = paste0(variable, "\n", "n=", num)) %>%
  ggplot( aes(x = myaxis, y = value, fill = variable)) +
  geom_violin(width = 0.7, alpha = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name ="Answers", limits = c(1,5)) +
  scale_fill_manual(values=c("#56B4E9")) +
  theme_minimal()


# Frequentist Wilcoxon-Signed Rank Tests
wilcox.test(alldata$B, alldata$EE, paired = T)
wilcox.test(alldata$PNB, alldata$NE, paired = T)

#-------------------------------------------------------------------------------
#                                                               
#                      #### 4. Regression Analysis ####
#                                                               
#-------------------------------------------------------------------------------

lm1 <- lm(B ~ NE_cent + EE_cent, alldata)
summary(lm1)
plot(lm1)

# check assumptions
check_model(lm1)

## homogeneity of variances
alldata$sqrd.resid <- abs(residuals(lm1))^2 # absolute value of squared residuals
levene <- lm(sqrd.resid ~ residence, data=alldata) 
anova(levene) # significant -> heterogenous variances

## linearity 
### plot residuals vs observed
plot(resid(lm1), alldata$B) # linearity seems fine


# plot linear models

tiff("LinearModelNE.tiff", width = 4, height = 4, units = 'in', res = 200)
ggplot(alldata, aes(NE_cent, B)) +
  lims(x = c(-2, 2), y = c(0, 6)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Normative Expectations") +
  ylab("Behaviour") + 
  theme_minimal()
dev.off()

tiff("LinearModelEE.tiff", width = 4, height = 4, units = 'in', res = 200)
ggplot(alldata, aes(EE_cent, B)) +
  lims(x = c(-2, 2), y = c(0, 6)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Empirical Expectations") +
  ylab("Behaviour") + 
  theme_minimal()
dev.off()

#-------------------------------------------------------------------------------
#                                                               
#                      #### 5. Conversations ####
#                                                               
#-------------------------------------------------------------------------------

# rename levels to combine data for the same neighbourhood
levels(alldata$residence) <- c("Bloemenbuurt", "De Linie", "Oosterpoort",
                               "Oosterpoort", "Piccardthof", "Oosterpoort", 
                               "Reitdiep", "Van Starkenborgh", "Van Starkenborgh")


alldata %>% 
  
  group_by(residence) %>% 
  
  summarise(Mean = mean(conversations, na.rm = T), 
            N = n(), 
            Min = min(conversations, na.rm = T),
            Max = max(conversations, na.rm = T))