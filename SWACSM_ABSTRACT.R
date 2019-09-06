
library(tidyverse)
library(modelr)
library(ggplot2)
df <- read.csv("../MetaData_exc.csv", stringsAsFactors = FALSE)
names(df)
glimpse(df)
glimpse(Trials)

# data(mtcars)
# mtcars <- mtcars
# glimpse(mtcars)
#Remove any controls or NA
Trials <- subset(df, Average_VO2 != "NA")
Trials <- subset(Trials, Spatial_reasoning_time != "NA")
Trials <- subset(Trials, Sex != "")


#Separate the genders
# MaleT <- subset(Trials, Sex == "M")
# FemaleT <- subset(Trials, Sex == "F")
# Trials %>% cbind(Sex = MaleT + FemaleT)

MaleT<- grep("^M", Trials$Sex)
FemaleT<- grep("^F", Trials$Sex)
glimpse(Trials$Sex)


# Changing the classifications
Trials$Height <- as.numeric(Trials$Height)
Trials$Weight <- as.numeric(Trials$Weight)


################# Testing Average VO2 Max ~  #######################
#######################################################################

# Quick Plot Average VO2 Max ~ Sex
 # p = ggplot(Trials, aes(x = Sex, y = Average_VO2)) +
 #   geom_point() 
 # p

# Linear modeling of relationships with Average VO2 Max
VO2M1 = lm(Average_VO2 ~ Sex, data = Trials)
summary(VO2M1)

# Gather info from model
Trials$predicted <- predict(VO2M1)
Trials$residuals <- residuals(VO2M1)

# Plot from above but with model precited values
# p2 = p + geom_smooth(method="lm", se=FALSE, alpha=.05, color="lightgrey") +
#   geom_point(aes(y = Trials$predicted),color="Red")
# p2

# Show residuals
# p2 + geom_segment(aes(xend = Sex, yend = Trials$predicted), linetype=2) +
#   theme_bw() 

## Check validity ##
# Residuals 
eps <- residuals(VO2M1) 
qqnorm(eps) 
qqline(eps) 

# Homoscedasticity 
yhat <- fitted(VO2M1) 
plot(yhat,eps) 
abline(h=0) 


### Testing Average VO2 Max ~ Sex + Age ####

# Quick Plot Average VO2 Max ~ Sex
# q = ggplot(Trials, aes(x = Age, y = Average_VO2, color = factor(Sex))) +
#   geom_point() 
# q

# Linear modeling of relationships with Average VO2 Max
VO2M2 = lm(Average_VO2 ~ Sex + Age, data = Trials)
summary(VO2M2)

# Gather info from model
Trials$predicted <- predict(VO2M2)
Trials$residuals <- residuals(VO2M2)

# Plot from above but with model precited values
# q2 = q + geom_smooth(method="lm", se=FALSE, alpha=.05, color="lightgrey") +
#   geom_point(aes(y = Trials$predicted),color="Red")
# q2

# Show residuals
# q2 + geom_segment(aes(xend = Age, yend = Trials$predicted), linetype=2) +
#   theme_bw() 

## Check validity ##
# Residuals 
eps <- residuals(VO2M2) 
qqnorm(eps) 
qqline(eps) 

# Homoscedasticity 
yhat <- fitted(VO2M2) 
plot(yhat,eps) 
abline(h=0) 


### Testing Average VO2 Max ~ Sex + Age + Height####

# Quick Plot Average VO2 Max ~ Sex + Age + Height
# r = ggplot(Trials, aes(x = Height, y = Average_VO2, color = Age, shape = Sex)) +
#   geom_point() 
# r

# Linear modeling of relationships with Average VO2 Max + Age + Height
VO2M3 = lm(Average_VO2 ~ Sex + Age + Height, data = Trials)
summary(VO2M3)

# Gather info from model
Trials$predicted <- predict(VO2M3)
Trials$residuals <- residuals(VO2M3)

# Plot from above but with model precited values
# r2 = r + geom_smooth(method="lm", se=FALSE, alpha=.05, color="lightgrey") +
#   geom_point(aes(y = Trials$predicted),color="Red")
# r2

# Show residuals
# r2 + geom_segment(aes(xend = Height, yend = Trials$predicted), linetype=2) +
#   theme_bw() 

## Check validity ##
# Residuals 
eps <- residuals(VO2M3) 
qqnorm(eps) 
qqline(eps) 

# Homoscedasticity 
yhat <- fitted(VO2M3) 
plot(yhat,eps) 
abline(h=0) 


### Testing Average VO2 Max ~ Sex + Age + Height + Weight ####

# Quick Plot Average VO2 Max ~ Sex + Age + Height + Weight
  # How do I figure out adding height to this or multiple variables??
# s = ggplot(Trials, aes(x = Weight, y = Average_VO2, color = Age, shape = Sex)) +
#   geom_point() 
# s

# Linear modeling of relationships with Average VO2 Max + Age + Height
VO2M4 = lm(Average_VO2 ~ Sex + Age + Height + Weight, data = Trials)
summary(VO2M4)

# Gather info from model
Trials$predicted <- predict(VO2M4)
Trials$residuals <- residuals(VO2M4)

# Plot from above but with model precited values
# s2 = s + geom_smooth(method="lm", se=FALSE, alpha=.05, color="lightgrey") +
#   geom_point(aes(y = Trials$predicted),color="Red")
# s2

# Show residuals
# s2 + geom_segment(aes(xend = Weight, yend = Trials$predicted), linetype=2) +
#   theme_bw() 

## Check validity ##
# Residuals 
eps <- residuals(VO2M4) 
qqnorm(eps) 
qqline(eps) 

# Homoscedasticity 
yhat <- fitted(VO2M4) 
plot(yhat,eps) 
abline(h=0) 




################# Testing Fitness Level ~  #######################
#######################################################################

# Quick Plot Fitness Level ~ Sex
# a = ggplot(Trials, aes(x = Sex, y = Fitness_Level)) +
#   geom_point() 
# a

# Linear modeling of relationships with Fitness level
Fit1 = lm(Fitness_Level ~ Sex, data = Trials, na.action = na.omit)
summary(Fit1)

# Gather info from model
Trials$predicted <- predict(VO2M1)
Trials$residuals <- residuals(VO2M1)

# Plot from above but with model precited values
# p2 = p + geom_smooth(method="lm", se=FALSE, alpha=.05, color="lightgrey") +
#   geom_point(aes(y = Trials$predicted),color="Red")
# p2

# Show residuals
# p2 + geom_segment(aes(xend = Sex, yend = Trials$predicted), linetype=2) +
#   theme_bw() 

## Check validity ##
# Residuals 
eps <- residuals(Fit1) 
qqnorm(eps) 
qqline(eps) 

# Homoscedasticity 
yhat <- fitted(Fit1) 
plot(yhat,eps) 
abline(h=0) 


################# ABC ~  #######################
#######################################################################
Abc1 = lm(ABC ~ Sex, data = Trials)
summary(Abc1)
## Check validity ##
# Residuals 
eps <- residuals(Abc1) 
qqnorm(eps) 
qqline(eps) 

# Homoscedasticity 
yhat <- fitted(Abc1) 
plot(yhat,eps) 
abline(h=0) 

# ABC ~ Sex Fitness Level
Abc2 = lm(ABC ~ Sex + Fitness_Level, data = Trials)
summary(Abc2)
## Check validity ##
# Residuals 
eps <- residuals(Abc2) 
qqnorm(eps) 
qqline(eps) 

# Homoscedasticity 
yhat <- fitted(Abc2) 
plot(yhat,eps) 
abline(h=0) 

###########################################################

#Quick Plotting
# hist(Trials$ILP_total)
# hist(Trials$Average_VO2)


#########Make data summaries, table with variables, age group variables



