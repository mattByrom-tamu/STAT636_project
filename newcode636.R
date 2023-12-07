library(gbm)
library(Metr)
library(tree)
library(ISLR2)
library(readxl)
df1 <- read_excel("C:\\Users\\haley\\Downloads\\clean_data.xlsx")
df = subset(df1, df1$Target != "Enrolled")

for(i in 1:nrow(df)){
  if(df$Target[i]=="Dropout"){
    df$Target[i]="0"
  }
}
for(i in 1:nrow(df)){
  if(df$Target[i]=="Graduate"){
    df$Target[i]="1"
  }
}
df$Target <- as.numeric(as.character(df$Target))
df$C1grade <- as.factor(df$C1grade)
df$C2grade <- as.factor(df$C2grade)
trainSize = dim(df)[1]/2 
train = sample(1:dim(df)[1], trainSize)
test = -train
dfTrain = df[train,]
dfTest = df[test,]

# frequency distribution chart of data grouped by 3 classifications found in column 'Target'
library(ggpubr)
> ggplot(df, aes(Target)) +
  +     geom_bar(fill = "white", color = "black") +
  +     theme_pubclean()


#Tree#
# reproducability
set.seed(1)
# boosting
#train GBM model - the best cross-validation interation was 254. There were 36 predictors, of which 10 had non-zero influence
boost.df <- gbm(Target ~ ., data=dfTrain, distribution = "gaussian", cv.folds=10,  n.trees=2000, interaction.depth=3, shrinkage=0.01)
print(boost.df)

# model performance GRAPH -- optimal # of trees: 254
perf_gbm <- gbm.perf(boost.df, method="cv")
print(perf_gbm)

# find index for number of trees with minimum cv error: 254
best <- which.min(boost.df$cv.error)

# prediction on TEST set -- MSE: 
library(Metrics)
predictdf <- predict(object=boost.df, newdata = dfTest, n.trees = perf_gbm)
summary(predictdf)

# Root MSE: 0.3314961
rmse(actual=dfTest$Target, predicted=predictdf)

# MSE: 0.1098896
mean((predictdf - dfTest$Target)^2)
