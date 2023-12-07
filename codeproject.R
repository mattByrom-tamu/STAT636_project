# Haley's code
install.packages("tree")
library(tree)
library(ISLR2)
library(readxl)
data <- read_excel("C:/Users/haley/Downloads/clean_data.xlsx")
subset <- read_excel("C:/Users/haley/Downloads/clean_data_subset.xlsx")

#Simple EDA, see Python html for in depth EDA#
dim(data)
str(data)
colnames(data)
head(data)
sum(is.na(data))
summary(data)
sapply(data, class)

#Tree#
mean(subset$Rate_unemployment)
attach(subset)
High <- factor(ifelse(Rate_unemployment <= 12, "No", "Yes"))
subset <- data.frame(subset, High)
tree.subset <- tree(High ~ . - Rate_unemployment, subset)
summary(tree.subset)