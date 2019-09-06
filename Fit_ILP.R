library(tidyverse)
library(modelr)
library(ggplot2)
library(lme4)
library(tinytex)
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
Runs$ILP_Speaking <- as.numeric(Runs$ILP_Speaking)
Runs$ILP_Reading_and_writing <- as.numeric(Runs$ILP_Reading_and_writing)
Runs$ILP_time_management <- as.numeric(Runs$ILP_time_management)

# Quick Plots
SpeakingPlot <- ggplot(data = Runs, aes(x = Fitness_Level, y = ILP_Speaking)) +
  geom_bar(stat="identity")
SpeakingPlot

ReadingPlot <- ggplot(data = Runs, aes(x = Fitness_Level, y = ILP_Reading_and_writing)) +
  geom_bar(stat="identity")
ReadingPlot

ManagementPlot <- ggplot(data = Runs, aes(x = Fitness_Level, y = ILP_time_management)) +
  geom_bar(stat="identity")
ManagementPlot

# Test Fitness Level ~ ILP
RunsILP = lm(ILP_Speaking + ILP_Reading_and_writing + ILP_time_management ~ Fitness_Level, data = Runs)
summary(RunsILP)

## Check validity ##
# Residuals 
epsILP <- residuals(RunsILP) 
qqnorm(epsILP) 
qqline(epsILP) 

# Homoscedasticity 
yhatILP <- fitted(RunsILP) 
plot(yhatILP,epsILP) 
abline(h=0) 
 
