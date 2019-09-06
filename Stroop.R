library(tidyverse)
library(modelr)
library(ggplot2)
library(lme4)
df <- read.csv("../Sawyer_Research_Data_Analysis/Stroop_data.csv", stringsAsFactors = FALSE)
names(df)
glimpse(df)

#### Might need to add to data 
# # Separate the sex
# MaleT<- grep("^M", df$Sex)
# FemaleT<- grep("^F", df$Sex)


# Remove blanks from group 
df <- subset(df, Subject_ID != "NA")
df <- subset(df, Group != "NA")
df <- subset(df, Stroop_Test != "NA" )
df <- subset(df, Score != "NA" )
df <- subset(df, Time != "NA" )

glimpse(df)

# Change categories
df$Subject_ID <- as.factor(df$Subject_ID)
df$Group <- as.factor(df$Group)
df$Stroop_Test <- as.factor(df$Stroop_Test)
df$Score <- as.numeric(df$Score)
df$Time <- as.factor(df$Time)
Stroop1 <- grep("^Pre", df$Time)
Stroop2 <- grep("^Post", df$Time)

# Quick Plot
Stroop1_Plot <- ggplot(data = df, aes(x = Time, y = Score, fill = Group)) +
  geom_bar(stat="identity")
Stroop1_Plot

Stroop2_Plot <- ggplot(data = Stroop2, aes(x = Group, y = Score)) +
  geom_bar(stat="identity")
Stroop2_Plot

Stroops_Plot <- ggplot(data = df, aes(x = Group, y = Score, color = Stroop_Test)) +
  geom_bar(stat="identity")
Stroops_Plot





# Test Fitness Level ~ Pre_Stroop
StroopPre = lm(Stroop_Test ~ Group * Time, data = df)
summary(StroopPre)

## Check validity ##
# Residuals 
eps <- residuals(Runs_Pre_Stroop) 
qqnorm(eps) 
qqline(eps) 

# Homoscedasticity 
yhat <- fitted(Runs_Pre_Stroop) 
plot(yhat,eps) 
abline(h=0) 

