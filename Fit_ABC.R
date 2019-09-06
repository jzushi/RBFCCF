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
Runs$ABC <- as.numeric(Runs$ABC)

# Quick Plot
ABCPlot <- ggplot(data = Runs, aes(x = Fitness_Level, y = ABC)) +
  geom_bar(stat="identity")
ABCPlot

# Test Fitness Level ~ ABC
RunsABC = lm(ABC ~ Fitness_Level, data = Runs)
summary(RunsABC)

## Check validity ##
# Residuals 
epsABC <- residuals(RunsABC) 
qqnorm(epsABC) 
qqline(epsABC) 

# Homoscedasticity 
yhatABC <- fitted(RunsABC) 
plot(yhatABC,epsABC) 
abline(h=0) 


