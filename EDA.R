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
