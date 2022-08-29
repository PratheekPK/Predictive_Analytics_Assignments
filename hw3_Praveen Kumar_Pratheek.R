#
#  Answers to Homework Assignment 3
#  BA 288 Winter 2022
#  Murphy 1/28/22 Draft
#
#
#  Read in dataset 
#
list.files(path = ".")
dat <- read.csv("hw3_hour.csv")
str(dat)
head(dat)
#
#
#  Part a.  Read and prepare the dataset
#

mnth <- as.factor(dat$mnth)
season <- as.factor(dat$season)
hr <- as.factor(dat$hr)
wkday <- as.factor(dat$wkday)
weathersit <- as.factor(dat$weathersit)
tmp_mnth <- data.frame(model.matrix(~mnth-1))
tmp_season <- data.frame(model.matrix(~season-1))
tmp_hr <- data.frame(model.matrix(~hr-1))
tmp_wkday <- data.frame(model.matrix(~wkday-1))
tmp_weathersit <- data.frame(model.matrix(~weathersit-1))
dat1 <- cbind(dat[,c(15,4)], tmp_season[,1:3], 
              tmp_mnth[,1:11], dat[,c(9, 7)], 
              tmp_wkday[,1:6], tmp_hr[,1:23], 
              tmp_weathersit[,2:4], dat[,11:14])
rm(mnth, season, hr, wkday, weathersit,holiday,yr,workday)
rm(tmp_mnth, tmp_season, tmp_hr, tmp_wkday, tmp_weathersit)


reg.all <- lm(cnt ~ ., data = dat1)
summary(reg.all)

# wkday5 is the column which shows linear dependency
dat1$wkday5 <- NULL

#
#
#  Part b.  Training and test dataset
#


set.seed(52089055)
train <- sample(nrow(dat1),nrow(dat1)/2)
dat1.train <- dat1[train,]
dat1.test <- dat1[-train,]

reg.all2 <- lm(cnt ~ ., data = dat1.train)
summary(reg.all2)

# The R squared value for the training set is 68.58%.
# The residual std error for the training set is 101.5

yhat.test <- predict(reg.all2, dat1.test)
RSS.test <- sum((dat1.test$cnt-yhat.test)^2)
MSE.test <- RSS.test/nrow(dat1.test)
MSE.test
RMSE.test <- MSE.test^0.5
RMSE.test

sum.train <- summary(reg.all2)
RMSE.train <- sum.train$sigma
RMSE.train

# The residual std error for the test set is 102.29
# The residual std error for the test set is slightly
# worse than the residual std error for the training set.

#
#
#  Part c.  Best regression model
#


dat1.best.test <- dat1.test

dat1.best.train <- dat1.train

dat1.best.test$weathersit4 <- NULL
dat1.best.test$hr6 <- NULL
dat1.best.test$wkday3 <- NULL
dat1.best.test$wkday2 <- NULL
dat1.best.test$wkday1 <- NULL
dat1.best.test$workday <- NULL
dat1.best.test$mnth11 <- NULL
dat1.best.test$mnth6 <- NULL
dat1.best.test$mnth7 <- NULL
dat1.best.test$mnth8 <- NULL
dat1.best.test$mnth1 <- NULL


dat1.best.train$weathersit4 <- NULL
dat1.best.train$hr6 <- NULL
dat1.best.train$wkday3 <- NULL
dat1.best.train$wkday2 <- NULL
dat1.best.train$wkday1 <- NULL
dat1.best.train$workday <- NULL
dat1.best.train$mnth11 <- NULL
dat1.best.train$mnth6 <- NULL
dat1.best.train$mnth7 <- NULL
dat1.best.train$mnth8 <- NULL
dat1.best.train$mnth1 <- NULL


reg.all.best <- lm(cnt ~ ., data = dat1.best.train)
summary(reg.all.best)
summary(reg.all2)

# The R squared value is 0.6858 and the residual std error
# is 101.5 for the all in regression.
# The R squared value is 0.6848 and the residual std error
# is 101.5 for the "best" regression.
# The number of columns has decreased from 53 to 42 but
# the R squared value has decreased only marginally and 
# the residual std error has not increased at all. Therefore
# it is the best regression model.


yhat.test.best <- predict(reg.all.best, dat1.best.test)
RSS.test <- sum((dat1.best.test$cnt-yhat.test.best)^2)
MSE.test <- RSS.test/nrow(dat1.best.test)
MSE.test
RMSE.test <- MSE.test^0.5
RMSE.test

# The RMSE on the test data is 102.3942

#
#
#  Part d.  LOO,5-Fold, 10 fold cross validation
#

library(boot)

glm.1 <- glm(cnt ~ ., data = dat1.best.train)
summary(glm.1)

RMSE.glm.1 <- ((sum(glm.1$residuals^2)/glm.1$df.residual))^0.5
RMSE.glm.1

cv.err <- cv.glm(dat1.best.train, glm.1)

cv.err$delta

MSE.LOOCV <- cv.err$delta[2]
RMSE.LOOCV <- MSE.LOOCV^0.5
RMSE.LOOCV


cv.err.5 <- cv.glm(dat1.best.train, glm.1, K = 5)
MSE.5 <- cv.err.5$delta[2]
RMSE.5 <- MSE.5^0.5
RMSE.5


cv.err.10 <- cv.glm(dat1.best.train, glm.1, K = 10)
MSE.10 <- cv.err.10$delta[2]
RMSE.10 <- MSE.10^0.5
RMSE.10

MSE.table <- data.frame(matrix(0,1,4))
names(MSE.table) <- c("Valid Set", 
                      "LOOCV", "5-Fold", "10-Fold")
MSE.table[1,] <- c(RMSE.test, RMSE.LOOCV, 
                   RMSE.5, RMSE.10)
MSE.table

# From this question we can see that the 5 fold cross
# validation has the lowest RMSE value followed by LOOCV
# and finally the 10-fold cross validation value.

#
#
#  Part e.  Regsubsets
#

library(leaps)
regfit.full <- regsubsets(cnt~., dat1.best.train,really.big = T,nvmax = 20)
#regfit.full.8 <- regsubsets(cnt~., dat1.best.train,really.big = T,nvmax = 8)

summary(regfit.full)
summary(regfit.full)$rsq
plot(summary(regfit.full)$rsq)

# The model has hr7 hr8 hr9 hr10 hr11 hr12 hr13 hr14 hr15 hr16 hr17 yr season1
# hr18 hr19 hr20 hr21 hr22 atemp

# I think a nvmax value of 20 is the perfect value because according
# to the graph, the r squared value starts tapering off at the 20 
# index mark. The r squared value is 0.663 approximately. Even if we take
# 15 more variables, the r squared value increases by a max of 0.01.

dat.test.mat <- model.matrix(cnt~.,data = dat1.best.train)
coef5 <- coef(regfit.full,5)
yhat5 <- dat.test.mat[,names(coef5)] %*% coef5
MSE.bs5 <- mean((dat1.best.train$cnt - yhat5)^2)
RMSE.bs5 <- MSE.bs5^0.5
RMSE.REGSUB <- RMSE.bs5

# The RMSE value is 135.908


#
#
#  Part f.  Forward and backward stepwise regression
#


regfit.fwd <- regsubsets(cnt~.,data=dat1.best.train, nvmax = 42,method = "forward")
summary(regfit.fwd)
summary(regfit.fwd)$rsq
coef(regfit.fwd, 20)
# I picked 20 because a r squared value of 0.648 seems to be reasonable

# (Intercept)          yr     season1      mnth10         hr0         hr1         hr2 
#116.56412    85.29662   -49.36820    42.77979  -107.68261  -124.80558  -129.54097 
#hr3         hr4         hr5         hr7         hr8         hr9        hr12 
#-142.67828  -146.65224  -128.30901    69.42804   198.33269    55.49586    54.43846 
#hr16        hr17        hr18        hr19 weathersit3        temp         hum 
#102.79233   247.96277   227.85837   127.67597   -53.53482   228.93812  -133.71659 

dat.test.mat <- model.matrix(cnt~.,data = dat1.best.train)
coef5 <- coef(regfit.fwd,5)
yhat5 <- dat.test.mat[,names(coef5)] %*% coef5
MSE.bs5 <- mean((dat1.best.train$cnt - yhat5)^2)
RMSE.bs5 <- MSE.bs5^0.5
RMSE.fwd <- RMSE.bs5

# The RMSE value is 136.2042

regfit.bck <- regsubsets(cnt~.,data=dat1.best.train, nvmax = 42,method = "backward")
summary(regfit.bck)
summary(regfit.bck)$rsq
coef(regfit.bck, 20)
# I picked 20 because a r squared value of 0.663 seems to be reasonable


#(Intercept)          yr     season1         hr7         hr8         hr9        hr10 
#-81.74036    88.84623   -54.52912   182.17871   315.67043   178.49189   125.70436 
#hr11        hr12        hr13        hr14        hr15        hr16        hr17 
#156.79197   199.28252   197.98378   192.38491   188.03427   253.07812   396.45871 
#hr18        hr19        hr20        hr21        hr22 weathersit3       atemp 
#370.80974   264.52673   179.23758   127.19442    86.67089   -82.52963   218.29346 

dat.test.mat <- model.matrix(cnt~.,data = dat1.best.train)
coef5 <- coef(regfit.bck,5)
yhat5 <- dat.test.mat[,names(coef5)] %*% coef5
MSE.bs5 <- mean((dat1.best.train$cnt - yhat5)^2)
RMSE.bs5 <- MSE.bs5^0.5
RMSE.bck <- RMSE.bs5

# The RMSE value is 137.5582

# A good model in backward stepwise regression is not always a good model
# in forward stepwise regression. But in this case it is.

#
#
#  Part g.  Ridge regression
#

install.packages("ISLR")
library(ISLR)

install.packages("glmnet")
library(glmnet)

y <- dat1.best.train$cnt
X <- model.matrix(cnt~., dat1.best.train)[,-1]

y.test <- dat1.best.test$cnt
X.test <- model.matrix(cnt~., dat1.best.test)[,-1]

cv.out <- cv.glmnet(X, y, alpha = 0)
plot(cv.out)
names(cv.out)

bestlam = cv.out$lambda.min
bestlam
# The best lambda chosen was 7.324
log(bestlam)

ridge.mod <- glmnet(X, y, alpha = 0, 
                    lambda = bestlam, thresh = 1e-12)


ridge.pred <- predict(ridge.mod, s=bestlam, newx = X.test)
MSE.R.CV <- mean((ridge.pred-y.test)^2)
RMSE.R.CV <- MSE.R.CV^0.5
RMSE.R.CV

# The test RMSE is 102.9057

#
#
#  Part h.  Lasso regression
#

cv.out1 <- cv.glmnet(X, y, alpha = 1)
plot(cv.out1)
bestlam1 <- cv.out1$lambda.min
bestlam1
log(bestlam1)

# The lambda selected is 0.0823

lasso.mod <- glmnet(X, y, alpha=1, 
                    lambda=bestlam1, thresh = 1e-12)

lasso.pred <- predict(lasso.mod, s=bestlam1, 
                      newx = X.test)
MSE.L.CV <- mean((lasso.pred-y.test)^2)
RMSE.L.CV <- MSE.L.CV^0.5
RMSE.L.CV
coef(lasso.mod)

# The RMSE value for Lasso Regression is 102.3903


#
#
#  Part i.  Test MSEs
#

RMSE.table <- data.frame(matrix(0,1,11))
names(RMSE.table) <- c("Backwards stepwise regression,","Forwards stepwise regression","NVMAX=20","GLM model","Lasso Regression","Ridge Regression","LOOCV","5 fold CV","10 fold CV","Test data","Train data")


RMSE.table[1,] <- c(RMSE.bck, RMSE.fwd,RMSE.REGSUB, RMSE.glm.1,RMSE.L.CV,RMSE.R.CV, RMSE.LOOCV, RMSE.5, RMSE.10,RMSE.test,RMSE.train)
RMSE.table

# It can be seen from this table that the lasso and ridge regression
# values are extremely good. Also that automating model selection results in
# extremely good values and eliminates many of the unnecessary columns
# and must be used in order to find out which columns are important and which are not.
# Ridge and Lasso were also extremely useful to find out which columns
# are important and gave out RMSE values which were nearly identical to
# the all-in model.

#
#
#  Part j.  Summarize
#

# 1) The peak times are from 7am-9am and 4pm-7pm
# 2) The business has been increasing with each year
# 3) Mist is bad for business but rain and snow is even worse.
# 4) High winds and humidity is also bad for business.
# 5) Normalised feeling temperature is more important than temperature
# for predicting the number of bike rentals.
# 6) September and October the business goes up


