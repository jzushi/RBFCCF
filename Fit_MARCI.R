library(tidyverse)
library(modelr)
library(ggplot2)
library(lme4)
library(emmeans)

df <- read.csv("../Sawyer_Research_Data_Analysis/MetaData_exc.csv", stringsAsFactors = FALSE)
names(df)
glimpse(df)

# Separate the sex
MaleT<- grep("^M", df$Sex)
FemaleT<- grep("^F", df$Sex)


# Remove Controls and blanks from group 
df <- subset(df, Sex != "")
df <- subset(df, Subject != "NA")
df <- subset(df, MARCI != "NA")
df <- subset(df, Fitness_Level != "NA")



Runs <- subset(df, Group != "Control")
glimpse(Runs)

# Change categories
Runs$Fitness_Level <- as.factor(Runs$Fitness_Level)
Runs$MARCI <- as.numeric(Runs$MARCI)

# Quick Plot
MARCIPlot <- ggplot(data = Runs, aes(x = Fitness_Level, y = MARCI)) +
  geom_bar(stat="identity")
MARCIPlot

# Test Fitness Level ~ MARCI
RunsMarci = lm(MARCI ~ Fitness_Level, data = Runs)
summary(RunsMarci)

## Check validity ##
# Residuals 
epsMarci <- residuals(RunsMarci) 
qqnorm(epsMarci) 
qqline(epsMarci) 

# Homoscedasticity 
yhatMarci <- fitted(RunsMarci) 
plot(yhatMarci,epsMarci) 
abline(h=0) 

emmeans(RunsMarci)
# # emmeans
# example warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
# example emmeans (warp.lm,  ~ wool | tension)
# 
# emmeans(RunsMarci, ~ MARCI | Fitness_Level)
