# EDA 

#packages and sourcing
source("R_functions.R")

# install.packages("tidyverse")
install.packages("GGally")
install.packages("vtable")
install.packages("reshape")
install.packages("rcompanion")
install.packages("caret")
install.packages("glmnet")
library(rcompanion)
library(reshape)
library(GGally)
library(tidyverse)
library(ggplot2)
library(vtable)
library(caret)
library(glmnet)
# import dataset 
df = read.csv("valentim_Academic_Success.csv", sep = ";") #Semicolon separator 

# set data types of variables, (factor(Categorical), int/num = numerical)
df$Marital.status <- factor(df$Marital.status)
cat_ColNames <- names(df[-c(7, 13, 22:36)]) # age is 20, could be considered ordinal. Includes response variable (Target) 
continuous_ColNames <- names(df[c(7, 13, 20, 22:36)]) # all of the predictors not in cat_ColNames
predictors <- names(df[1:36])
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
  boxplot(df[[i]] ~ df$Target, main = as.character(i), ylab = "Countinous Var Count", xlab = "Academic Success") # boxplots broken out by target variable
}

boxplot(df[GDP] ~ df$Target, main = as.character(i))

# Correlation: Cramer's V or phi squared for categorical data (keep in mind most variables are categorical)
corCont = round(cor(continuousPred),2) # rounded
melted_corCont = melt(get_lower_tri(corCont), na.rm = TRUE)

#heatmap for cont variable
ggplot(data = melted_corCont, aes(x=X1, y = X2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()  
  
# Cramers V having issues with Cramer's V. Looks like you have to run it with a contingency table rather than a DF. Can look into this later


# summary statistic comparisons based on target partitions 
# make df of the different target variables
dfDropout = subset(df, df$Target == "Dropout")
dfEnrolled = subset(df, df$Target == "Enrolled")
dfGraduate = subset(df, df$Target == "Graduate")

sumtable(df, group = 'Target', group.test = TRUE)

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


# logistic regression 
dfNew = subset(df, df$Target != "Enrolled") # Removing enrolled so we have a simpler regression problem
# make the Target 1 (Graduate) and 0 (Drop out)
dfNew$Target
y = ifelse(df$Target == "Graduate", 1 , 0)
x = model.matrix(Target~., df)[,-1]

fit <- glm(Target ~ ., family = binomial(link = logit), data = df)
summary(fit)

# Example from Cat TB

 fit <- glm(Target ~ ., family = binomial, data = dfNew)
 summary(fit)

fit.LASSO <- glmnet(x, y, alpha = 1, family = "binomial")
plot(fit.LASSO, "lambda")

cvLASSO <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "class")
cvLASSO$lambda.min # optimal lambda 
coef(cvLASSO, s='lambda.1se')
plot(cvLASSO)

coef(glmnet(x, y, alpha = 1, family = "binomial", lambda = ))


attach(Students)
