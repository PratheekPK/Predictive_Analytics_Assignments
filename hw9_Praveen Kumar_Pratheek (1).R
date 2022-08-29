#
#  Answers to Homework Assignment 9
#  BA 288 Winter 2022
#  Murphy 3/18/22 Draft
#
#
#  Read in dataset 
#

dat <- read.csv("spam.csv")
head(dat)

#
#
#  1.	How is spam email normally detected?  
#     What are key variables that might indicate that an email is spam?  
#     Provide a few brief sentences of insight. 
#
# 


# An email which is classified as a spam email will usually have a 
# very suspicious ip address.
# Spam emails also used certain words which do not appear frequently in
# normal emails.The percentage of occurrence of spam words are very high too in spam messages.
# Topic modeling has also been used on the ‘cleaned’ text 
# to discover the topics that occur in the collection of text.
# Some key variables to detect spam could be the frequency of commonly used spam trigger words
# like 1. Act now 2. Action 3. Apply now 4. Apply online
# Also in spam emails consecutive capital letters occur quite often.


#
#
#  2. Set a seed.  
# Create training and test data sets.  
# Create the training data set with 2000 observations equally balanced between 
# spam and non-spam observations.  
# Create a test data set with 800 observations again equally balanced.
#
# 

set.seed(112233)

succ <- subset(dat, dat$spam == 1)
fail <- subset(dat, dat$spam == 0)

train.succ <- sample(1:nrow(succ),1000)
train.fail <- sample(1:nrow(fail),1000)

dat.train <- rbind(succ[train.succ,],fail[train.fail,])
str(dat.train)
dim(dat.train)

newfail <- fail[-train.fail,]

test.fail <- newfail[sample(1:nrow(newfail),400),]

newsucc <- succ[-train.succ,]

test.succ <- newsucc[sample(1:nrow(newsucc),400),]

dat.test <- rbind(test.succ,test.fail)



#
#
#  3.Build a logistic regression model using all the variables 
# in the training data set.  Comment on the quality of the model with 
# respect to classification on the training data.  Make predictions 
# on the test data, how does the model fitted on the training data perform 
# with respect to classification on this data set?
#
# 


logreg <- glm(spam ~ . , data = dat.train, family = "binomial")
summary(logreg)

yhat.all.train <- predict(logreg, dat.train, 
                          type = "response")  
yhat.all.train.cl <- ifelse(yhat.all.train > 0.5, 1, 0)
tab.all.train <- table(dat.train$spam, 
                       yhat.all.train.cl, 
                       dnn = c("Actual","Predicted"))
tab.all.train

(tab.all.train[1,1]+tab.all.train[2,2])/(tab.all.train[2,1]+tab.all.train[1,2]+tab.all.train[1,1]+tab.all.train[2,2])


# The accuracy on the training dataset is 92.65% which is extremely good.

yhat.all.test <- predict(logreg, dat.test, 
                          type = "response")  
yhat.all.test.cl <- ifelse(yhat.all.test > 0.5, 1, 0)
tab.all.test <- table(dat.test$spam, 
                       yhat.all.test.cl, 
                       dnn = c("Actual","Predicted"))
tab.all.test

(tab.all.test[1,1]+tab.all.test[2,2])/(tab.all.test[2,1]+tab.all.test[1,2]+tab.all.test[1,1]+tab.all.test[2,2])

# The accuracy on the test dataset is 91.5 which is almost as good as the accuracy on the 
# training dataset.


#
#
#  4. Copy the data to a new data frame for this problem.  
# Normalize the independent variables. 
# Run k-nearest neighbor on the spam data for k = 1 to 99.  
# What value of k gives the lowest test set error?
#
# 

dat1 = dat

#rm(dat.x,dat.y)

dat.train.x <- dat.train[,1:57]
dat.train.y <- dat.train[,58]
dat.test.x <- dat.test[,1:57]
dat.test.y <- dat.test[,58]

dat.train.x <- scale(dat.train.x)
dat.test.x <- scale(dat.test.x)

list_of_knns <- list()
library(class)
for (val in 1:99){
  out1 <- knn(dat.train.x, dat.test.x, dat.train.y, k=val)
  knn1.err <- mean(dat.test.y != out1)
  list_of_knns[[val]] <- knn1.err
  
}

which.min(unlist(list_of_knns))

# The value of k which gives the lowest test set error is 10.


#
#
#  5.Create a pruned classification tree on the spam training data.  
# What tree provides the lowest test data set error?  
# What is the error level?
#
# 

library(tree)

dat.train.2 = dat.train
dat.test.2 = dat.test

dat.train.2[,58] <- as.factor(dat.train.2[,58])
dat.test.2[,58] <- as.factor(dat.test.2[,58])

tree1 <- tree(spam~., data = dat.train.2)
summary(tree1)

plot(tree1)
text(tree1, pretty = 0)

prune1 <- prune.misclass(tree1)
names(prune1)

plot(prune1)
plot(prune1$size, prune1$dev, xlab = "Size of Tree",
     ylab = "Deviation")

prune.tree1 <- prune.misclass(tree1, best = 6)
summary(prune.tree1)
prune.tree1
plot(prune.tree1)
text(prune.tree1, pretty = 0)

# Tree1 provides the lowest misclassification error rate of 0.0835.
# However the pruned tree provides a misclassification error rate of
# 0.126

#
#
#  6.	Create a support vector machine (SVM) classifier 
# for the training data using different kernals: linear, radial, polynomial.  
# What is the SVM that provides the best fit on the test data?  
# What is the error level?
#
#
# 

library(e1071)

dat.train6 <- dat.train.2
dat.test6 <- dat.test.2
dat.train6$spam <- as.factor(dat.train6$spam)
dat.test6$spam <- as.factor(dat.test6$spam)

set.seed(123789)
tune.out.linear <- tune(svm, spam~., data = dat.train6, 
                        kernel = "linear", 
                        ranges = list(cost = c(0.001, 0.01, 0.1, 
                                               1, 5, 10, 100)))
summary(tune.out.linear) 
svm.linear <- tune.out.linear$best.model
summary(svm.linear)
svm.pred.test.lin = predict(svm.linear, dat.test)
table(truth = dat.test6$spam, predicted = svm.pred.test.lin)
err.svm.lin <- mean(dat.test6$spam != svm.pred.test.lin)
err.svm.lin
# 0.085

tune.out.radial <- tune(svm, spam~., data = dat.train6, 
                        kernel = "radial", 
                        ranges = list(cost = c(0.001, 0.01, 0.1, 
                                               1, 5, 10, 100)))
summary(tune.out.radial) 
svm.radial <- tune.out.radial$best.model
summary(svm.radial)
svm.pred.test.rad = predict(svm.radial, dat.test)
table(truth = dat.test6$spam, predicted = svm.pred.test.rad)
err.svm.rad <- mean(dat.test6$spam != svm.pred.test.rad)
err.svm.rad
# 0.07625

tune.out.poly <- tune(svm, spam~., data = dat.train6, 
                        kernel = "polynomial", 
                        ranges = list(cost = c(0.001, 0.01, 0.1, 
                                               1, 5, 10, 100)))
summary(tune.out.poly) 
svm.poly <- tune.out.poly$best.model
summary(svm.poly)
svm.pred.test.poly = predict(svm.poly, dat.test)
table(truth = dat.test6$spam, predicted = svm.pred.test.poly)
err.svm.poly <- mean(dat.test6$spam != svm.pred.test.poly)
err.svm.poly
# 0.0775

# The SVM which provides the best fit is radial
# The error level of the linear SVM is 0.07625



#
#
#  7.	Build a neural network for classifying each observation as spam.  
# Use a single hidden layer.  Try different values for the number of nodes 
# in the hidden layer, 10, 20 and 30.  How does the neural network perform with 
# respect to predicting spam? 
#
#
# 

library(neuralnet)
library(nnet)

predvars <- colnames(dat.train.2[1:57])
predvars.form <- paste(predvars, collapse = "+")
form <- as.formula(paste("spam~",predvars.form))
form

nn.out <- neuralnet(form, dat.train, hidden = 10, 
                    linear.output = FALSE,
                    lifesign = "minimal")

nn.out
plot(nn.out)

nn.test.pred.10 <- compute(nn.out, dat.test[,1:57])

nn.test.10 <- ifelse(nn.test.pred.10$net.result > 0.5, 1, 0)
table(truth = dat.test$spam, predicted = nn.test.10)
err.nn10 <- mean(dat.test$spam != nn.test.10)
err.nn10
# 0.08


#
#
#  8.	Try at least one neural network with two hidden layers.  
# How do these models perform on predicting spam on the test data?  
# Report classification errors.
#
#


nn.out.2 <- neuralnet(form, dat.train, hidden = c(10,10), 
                    linear.output = FALSE,
                    lifesign = "minimal")

nn.out.2
plot(nn.out.2)

nn.test.pred.10 <- compute(nn.out.2, dat.test[,1:57])

nn.test.10 <- ifelse(nn.test.pred.10$net.result > 0.5, 1, 0)
table(truth = dat.test$spam, predicted = nn.test.10)
err.nn10 <- mean(dat.test$spam != nn.test.10)
err.nn10
weights <- nn.out.2$weights[1]

# 0.0775

# The deeper the model is, the betetr the error rate is.



#
#
#  9.	Which model performed the best as a classifier on the spam data?  
# Rhetorical question, do you evaluate this on the training or test data set?  
# Provide insight as to why you believe that model performed best.  
# Are there any variables that can be seen as standing out as highly 
# predictive of spam email?
#


# The model which performed the best was the multiple hidden layer neural network.
# I evaluated this on the test data set because there is a possibility that with very
# deep neural networks, the model might overfit to the training dataset.
# Neural Networks can have a large number of free parameters 
# (the weights and biases between interconnected units) and this gives them 
# the flexibility to fit highly complex data (when trained correctly) that other 
# models are too simple to fit. This is why it performs the best.
# word_freq_george, word_freq_hp, word_freq_free, word_freq_remove, word_freq_our,
# word_freq_re, word_freq_edu are variables which are highly predictive of spam
# email.


#
#
#  10.	How many parameters (or model degrees of freedom) are in each of 
# the above models (parts 3, 4, 5, 6, 7 and 8)?  
# What does this have to do with model fit?  
# How does this relate to the bias-variance tradeoff?
#

# For logistic regression there are 58 degrees of freedome
# For KNN there are N/k degrees of freedom.
# For the pruned decision tree there are 6 degrees of freedom.
# When using SVM(/SVR), the degrees of freedom are in fact the number of training instances
# When using the neural network there are 701 for the 2 layer network.
# With the number of parameters being more, the probability of overfitting increases.
# The model will overfit to the training dataset and not perform well on the test dataset.
# With a higher number of parameters, the model will show less bias but high variance.







