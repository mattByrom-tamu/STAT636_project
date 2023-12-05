# EDA 

#packages and sourcing
source("R_functions.R")

# install.packages("tidyverse")
install.packages("GGally")
library(GGally)
library(tidyverse)
library(ggplot2)

# import dataset 
df = read.csv("valentim_Academic_Success.csv", sep = ";") #Semicolon separator 

# set data types of variables, (factor(Categorical), int/num = numerical)
df$Marital.status <- factor(df$Marital.status)
cat_ColNames <- names(df[-c(7, 13, 22:36)]) # age is 20, could be considered ordinal. Includes response variable (Target) 
continuous_ColNames <- names(df[c(7, 13, 20, 22:36)]) # all of the predictors not in cat_ColNames

# create subsets of data, categorical and continuous 
cat_Pred = df[-c(7, 13, 22:36)]
continuousPred = df[c(7, 13, 20, 22:36)]

# update data types/classes for the categorical variables. Other variable numeric. 
df[cat_ColNames] <- lapply(df[cat_ColNames], factor)
df[continuous_ColNames] <- lapply(df[continuous_ColNames], as.numeric)

# double check classes
custom_view(df)

# code to look for NA values
# is.na(df)
sum(is.na(df)) # no missing values, no imputation needed. Supported in dataset documentation as well

# summary statistics/tables
summary(df)

# histograms, box plots, bar charts(cat data)
for(i in cat_ColNames){
  # barplots % of observations 
  barplot(prop.table(table(df[i])), xlab = "categories", ylab = "% observations", main = as.character(i))
  # barplots with sum counts 
  print(ggplot(df) + geom_bar(aes_string(x = i)) + ggtitle(as.character(i)) +
    xlab("categories") +
    theme(plot.title = element_text(hjust = 0.5)))
}

# plots for continuous variables
for(i in continuous_ColNames){
  hist(prop.table(table(df[i])), xlab = "numerical value", main = as.character(i), breaks = 10) # change breaks for bins
}


# outliers (only applicable to continuous Variables) 

# contingency tables? 

# Correlation: Cramer's V or phi squared for categorical data (keep in mind most variables are categorical)

# note: haven't used Cramer's V before. But it's cited a bunch of places for categorical data 

# heatmap for continuous variables correlation

# summary statistic comparisons based on target partitions 

# 
# ggpairs(df, cardinality_threshold = 50)
ggpairs(cat_Pred) # variables of interest 



install.packages("tree")
library(tree)
library(ISLR2)
library(readxl)

#Tree#
mean(subset$Rate_unemployment)
attach(subset)
High <- factor(ifelse(Rate_unemployment <= 12, "No", "Yes"))
subset <- data.frame(subset, High)
tree.subset <- tree(High ~ . - Rate_unemployment, subset)
summary(tree.subset)


