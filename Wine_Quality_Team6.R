# Class: BANA 288: Predictive Analytics
# Assignment: Group Assignment: Red Wine Analysis: Predictive Team Project 
# Team 5: Chirag Madhukar, Noor Zia, Priya R, Parth Parsana, Pratheek PK
# Date: 02.28.22
suppressMessages(library(dplyr))
suppressMessages(library(caret))
suppressMessages(library(rpart.plot))
suppressMessages(library(rpart))
suppressMessages(library(randomForest))
#install.packages("ROSE")
library(ROSE)
library(nnet)
# Installing the package
#install.packages(dplyr)
#install.packages("corrplot")
library(corrplot)
# Loading the package
library(dplyr)
#install.packages('UBL')
library(UBL)
#install.packages("Hmisc")
library("Hmisc")
library(plotly)
library(corrplot)
install.packages("xgboost")
library(xgboost)
wine <- read.csv("winequality-red.csv")


res2 <- rcorr(as.matrix(wine))
res2

cor(wine, method = c("pearson", "kendall", "spearman"))
res <- cor(wine)
round(res, 2)
#Correlation Matrix

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

#Converted quality rating to 3 bins of taste

wine$taste <- ifelse(wine$quality <= 4, 'bad', ifelse(wine$quality> 4 & wine$quality<= 6,'Ok', 'Good'))

wine$taste <- as.factor(wine$taste)

inbalanced_train <- createDataPartition(wine$taste, p=.7, list = F)

balanced_train <- wine[inbalanced_train,]
testrf <- wine[-inbalanced_train,]
names(balanced_train)

# balance the dataset as there is imbalance in distribution class taste
table(balanced_train$taste)

# use SmoteClassifier to oversample dataset
balanced_train <- SmoteClassif(taste~.-quality, balanced_train, C.perc = "balance")
table(balanced_train$taste)
str(balanced_train)
balanced_train$taste <- as.factor(balanced_train$taste)


## volatile, chloride, free and total SO2, pH, sulphates, alcohol
cor(wine)[12,]

# The quality is dependent on alcohol,volatile acidity and density
# content at p<0.05

# alcohol, volatile, sulphates
# Final 3 - alcohol, volatile, sulphates

mydata <- cbind(balanced_train[,c(2,10,11)])

################# Multinomial Logistic Regression##########################
# The multiple independent variables call for a multiple logistic regression 
# to find the statistical significance of the independent variables effect on
# the dependent variable "Taste"
# Fit the model
multmodel <- multinom(taste~.-quality, data=balanced_train)
# Summarize the model
summary(multmodel)

# Calculate z-values
zvalues <- summary(multmodel)$coefficients / summary(multmodel)$standard.errors
# Show z-values
zvalues

# To interpret results, we will find pvalues from log regression output
pvalues<- pnorm(abs(zvalues), lower.tail=FALSE)*2
head(pvalues)

# Make predictions
predicted.classes <- multmodel %>% predict(testrf)
head(predicted.classes)
# Model accuracy
confusionMatrix(predicted.classes, testrf$taste)

############################Random Forest##############################

# dependent class is converted to  3 class from 10 ,
# Quality <= 4 => bad 
# Quality > 4 and Quality <= 6=> Ok 
# Quality > 6 => good 
set.seed(12345)

corrplot(cor(balanced_train), method = "circle")


barplot(table(balanced_train$taste))
# Random Forest is an ensemble model and should perform better with 3 class
# Training with classification tree
# Variable selection happens within the model
# selected variables are 3

fitrf <- randomForest(taste ~ alcohol+volatile.acidity+sulphates+citric.acid+
                      density+pH, data=balanced_train,
                      ,mtry=3,ntree=100,max.depth= 6)
print(fitrf)
# Predict the testing set with the trained model 
predictionsrf <- predict(fitrf, testrf, type = "class")

# Accuracy and other metrics
confusionMatrix(predictionsrf, testrf$taste)

importance(fitrf)
varImpPlot(fitrf)

error_metric=function(mat)
{
  
  
  precision = diag(mat) / rowSums(mat)
  recall <- (diag(mat) / colSums(mat))
  
  print(paste("Precision value of the model: ",precision))
  print(paste("Recall of the model: ",recall))
  
  
}

error_metric(table(predictionsrf, testrf$taste))
# The 3 variables of importance that help classifies the taste are
# alcohol, sulphates and volatile acidity

# As a balanced dataset, Random forest prediction models are doing good at 
# an accuracy of 75%. 

#############################Support Vector Machine############################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, xgboost, h2o, corrplot, rpart.plot, corrgram, lightgbm, visNetwork)


str(wine)
library("knitr")
wine %>% skim() %>% kable()
?skim
colnames(wine) <- wine %>% colnames() %>% str_replace_all(" ","_")

svm_model <- svm(taste~alcohol+volatile.acidity+sulphates+citric.acid+
                   density+pH,balanced_train)

svm_result <- predict(svm_model, newdata = testrf[,!colnames(testrf) %in% c("taste")])

confusionMatrix(svm_result, testrf$taste)

error_metric(table(svm_result, testrf$taste))
# The advantages of support vector machines are:
#   
# Effective in high dimensional spaces.
# Still effective in cases where number of dimensions 
# is greater than the number of samples.

# In a balanced train dataset, SVM  models got an accuracy of  60%. 
########################### XGBoost ################################################
balanced_train$taste <- as.numeric(as.factor(balanced_train$taste)) -1
testrf$taste <- as.numeric(as.factor(testrf$taste)) -1
train_matrix  <- xgb.DMatrix(data = as.matrix(balanced_train[,c("alcohol","volatile.acidity","citric.acid","density","pH","sulphates")]), label = as.matrix(balanced_train["taste"]))
test_matrix  <- xgb.DMatrix(data = as.matrix(testrf[,c("alcohol","volatile.acidity","citric.acid","density","pH","sulphates")]), label = as.matrix(testrf["taste"]))
colnames(balanced_train)
bstDMatrix <- xgboost(num_class=3,data = train_matrix, nrounds = 100, objective = "multi:softmax")
bstDMatrix
wine$taste
xgb.pred = predict(bstDMatrix,as.matrix(testrf[,c("alcohol","volatile.acidity","citric.acid","density","pH","sulphates")]),reshape=T)

xgb.pred = as.data.frame(xgb.pred)
confusionMatrix(data=factor(xgb.pred[,1]), reference = factor(testrf["taste"][,1]))

xgb.result <- xgb.pred[,1]

xgb.result[xgb.result==0] = "bad"
xgb.result[xgb.result==1] = "Good"
xgb.result[xgb.result==2] = "Ok"

xgb.result = as.factor(xgb.result)

error_metric(table(xgb.result, testrf$taste))

xgb.importance(model = bstDMatrix)

# In a balanced train dataset, XgBoost  models got an accuracy of  74%. 
# When we tried  Xgboost on an imbalanced dataset , 
# it did better than Random Forest.

# Overall XgBoost and Random forest predict the taste
# of wine better than SVM. The sensitivity and specificity 
# is also good in both the models. The true classes 
# bad or good or ok are predicted more than 60% of the times 
# in all the models.

