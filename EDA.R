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

# code to look for NA values

# summary statistics/tables

# outliers (only aplicable to continuous) 

# histograms, box plots, bar charts(cat data)

# contingency tables? 

# Correlation: Crameer's V or phi squared for categorical data (keep in mind most variables are categorical)


