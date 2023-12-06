# EDA 
set.seed(1) # for reproducability 
#packages and sourcing
source("R_functions.R")

# install.packages("tidyverse")
# install.packages("GGally")
# install.packages("vtable")
# install.packages("reshape")
# install.packages("rcompanion")
# install.packages("caret")
# install.packages("glmnet")
# install.packages("BART")
# install.packages("gbm")
library(gbm)
library(BART)
library(rcompanion)
library(reshape)
library(GGally)
library(tidyverse)
library(ggplot2)
library(vtable)
library(caret)
library(glmnet)


# import dataset 
df_All= read.csv("valentim_Academic_Success.csv", sep = ";") #Semicolon separator 
df = subset(df_All, df_All$Target != "Enrolled") # Removing enrolled so we are only looking at unenrolled students

# set data types of variables, (factor(Categorical), int/num = numerical)
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

# NA values
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


# Correlation for continuous variables
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

# summary tables for dropout vs graduate
# make df of the different target variables
dfDropout = subset(df, df$Target == "Dropout")
# dfEnrolled = subset(df, df$Target == "Enrolled")
dfGraduate = subset(df, df$Target == "Graduate")
sumtable(df, group = 'Target', group.test = TRUE)

# set training and test data before modeling, did 50/50 split
trainSize = dim(df)[1]/2 # 20% of values for training set
train = sample(1:dim(df)[1], trainSize)
test = -train
dfTrain = df[train,]
dfTest = df[test,]

# summary(dfTrain) # checking the training and test df's have similar distributions 
# summary(dfTest)

# logistic regression 
# make the Target 1 (Graduate) and 0 (Drop out)
y = ifelse(df$Target == "Graduate", 1 , 0)
x = model.matrix(Target~., df)[,-1]

fitLog <- glm(y ~ x, family = binomial(link = logit))
summary(fitLog)

# fit LASSO
fit.LASSO <- glmnet(x, y, alpha = 1, family = "binomial")
summary(fit.LASSO)
plot(fit.LASSO, "lambda") # need to add legend

cvLASSO <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "class")
cvLASSO$lambda.min # optimal lambda 
cvLASSO$lambda.1se # given model is so complex this is a better 

coef(cvLASSO, cvLASSO$lambda.min) # minimum lambda coeficient values
coef(cvLASSO, cvLASSO$lambda.1se) # simplest model with lambda within 1 se coeficients 

# figure out how to print out test/train errors for the CV
plot(cvLASSO) # plot of log(lambda) as the model is shrunk. Shows lambda.min and lambda.1se
log(cvLASSO$lambda.min) # value of the leftmost dotted line on plot
log(cvLASSO$lambda.1se) # value of the 2nd (rightmost) line on the plot 

cvLASSO$glmnet.fit # plot of lambda values, df, and % deviance.
cvLASSO$index # index of lambda min and 1se in the matrix above 


# Tree modeling 
# Boosting model 

boostDF = gbm(Target ~ ., data = df,
              distribution = "gaussian", n.trees = 5000,
              cv.folds = 5,
              interaction.depth = 4)
summary(boostDF)
mean(boostDF$cv.error)

# testing different interaction depths 
for(i in 2:7){
  boostDF = gbm(Target ~ ., data = df,
                distribution = "gaussian", n.trees = 500,
                cv.folds = 5,
                interaction.depth = i)
  print(paste("depth ", i , " mean error: " ,mean(boostDF$cv.error)))
}

# test different n.trees 
# testing different interaction depths and shrinking parameters 
# I know this is bad R code so be careful, this will take a long time to run :) 
# create df to hold values
pickingTuners <- data.frame(depth = c(), nTrees = c(), Lambda = c(), meanError = c())
# pickingTuners <- rbind(pickingTuners, list(1, 3, 4, 5))
for(depth in 1:7){
for(lambda in c(.0001, .001, .01, .1, .2)) {
  for (i in c(50, 100, 150, 200, 300, 400, 500, 750, 1000, 1500, 2000)) {
    boostDF = gbm(
      Target ~ .,
      data = df,
      distribution = "gaussian",
      n.trees = i,
      cv.folds = 5,
      shrinkage = lambda,
      interaction.depth = depth
    ) # using 4 interaction trees
    pickingTuners <- rbind(pickingTuners, list(i, lambda, mean(boostDF$cv.error)))
    print(paste("depth: ", depth, "n.trees: ",i ,", shrinking parameter: ",lambda," mean error: " ,mean(boostDF$cv.error)))
  }
}
}

# going to use 
# testing 
