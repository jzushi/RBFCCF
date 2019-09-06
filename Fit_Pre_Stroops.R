library(tidyverse)
library(modelr)
library(ggplot2)
library(lme4)
df <- read.csv("../Sawyer_Research_Data_Analysis/MetaData_exc.csv", stringsAsFactors = FALSE)
names(df)
glimpse(df)

# Separate the sex
MaleT<- grep("^M", df$Sex)
FemaleT<- grep("^F", df$Sex)


# Remove Controls and blanks from group 
df <- subset(df, Sex != "")
df <- subset(df, Subject != "NA")
Runs <- subset(df, Group != "Control")
glimpse(Runs)

# Change categories
Runs$Fitness_Level <- as.factor(Runs$Fitness_Level)
Runs$Pre_stroop1 <- as.numeric(Runs$Pre_stroop1)
Runs$Pre_stroop2 <- as.numeric(Runs$Pre_stroop2)

# Quick Plot
Fit_Pre_Stroop1_Plot <- ggplot(data = Runs, aes(x = Fitness_Level, y = Pre_stroop1)) +
  geom_bar(stat="identity")
Fit_Pre_Stroop1_Plot

Fit_Pre_Stroop2_Plot <- ggplot(data = Runs, aes(x = Fitness_Level, y = Pre_stroop2)) +
  geom_bar(stat="identity")
Fit_Pre_Stroop2_Plot

# Model Test Fitness Level ~ Pre_Stroop
Runs_Pre_Stroop = lm(Pre_stroop1 + Pre_stroop2 ~ Fitness_Level, data = Runs)
summary(Runs_Pre_Stroop)

## Check validity ##
# Residuals 
epsPreStroop <- residuals(Runs_Pre_Stroop) 
qqnorm(epsPreStroop) 
qqline(epsPreStroop) 

# Homoscedasticity 
yhatPreStroop <- fitted(Runs_Pre_Stroop) 
plot(yhatPreStroop,epsPreStroop) 
abline(h=0) 

