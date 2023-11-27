# EDA 

#packages and sourcing
source("R_functions.R")

# install.packages("tidyverse")
library(tidyverse)

# import dataset 
df = read.csv("valentim_Academic_Success.csv", sep = ";") #Semicolon separator 

# set variable types 
custom_view(df)

# set data types of variables, (factor(Categorical), int/num = numerical)
df$Marital.status <- factor(df$Marital.status)
cat_ColNames <- names(df[-c(7, 13, 22:36)]) # age is 20, could be considered ordinal. Includes response variable (Target) 
continuous_ColNames <- names(df[c(7, 13, 22:36)]) # all of the predictors not in cat_ColNames

# code to look for NA values

# summary statistics/tables

# outliers (only aplicable to continuous) 

# histograms, box plots, bar charts(cat data)

# contingency tables? 

# Correlation: Crameer's V or phi squared for categorical data (keep in mind most variables are categorical)
# note: haven't used Cramer's V before. But it's cited a bunch of places for categorical data 
