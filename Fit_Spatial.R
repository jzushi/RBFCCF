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
df <- subset(df, Spatial_reasoning_time != "NA")
## Remove Spatial NA
Runs <- subset(df, Group != "Control")
glimpse(Runs)

# Change categories
Runs$Fitness_Level <- as.factor(Runs$Fitness_Level)
Runs$Spatial_reasoning_score <- as.numeric(Runs$Spatial_reasoning_score)
Runs$Spatial_reasoning_time <- as.numeric(Runs$Spatial_reasoning_time)

# Quick Plot
Spatial_Score_Plot <- ggplot(data = Runs, aes(x = Fitness_Level, y = Spatial_reasoning_score)) +
  geom_bar(stat="identity")
Spatial_Score_Plot

Spatial_Time_Plot <- ggplot(data = Runs, aes(x = Fitness_Level, y = Spatial_reasoning_time)) +
  geom_bar(stat="identity")
Spatial_Time_Plot

# Test Fitness Level ~ Spatial Reasoning 
RunsSpatial = lm(Spatial_reasoning_time + Spatial_reasoning_score ~ Fitness_Level, data = Runs)
summary(RunsSpatial)

## Check validity ##
# Residuals 
epsSpatial <- residuals(RunsSpatial) 
qqnorm(epsSpatial) 
qqline(epsSpatial) 

# Homoscedasticity 
yhatSpatial <- fitted(RunsSpatial) 
plot(yhatSpatial,epsSpatial) 
abline(h=0) 

