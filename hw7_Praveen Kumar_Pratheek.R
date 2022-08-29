#
#  Answers to Homework Assignment 7
#  BA 288 Winter 2022
#  Murphy 3/6/22 Draft
#
#
#  Read in dataset 
#

dat <- read.csv("hw7_bank_term_deposit_big.csv")

#
#
#  Part 1.  
#
# 

str(dat)

jb <-dat$job
mari <- dat$marital
ed <- dat$education
cntct <- dat$contact
m <- dat$month
pout <- dat$poutcome

tmp_job <- data.frame(model.matrix(~jb - 1))
tmp_marit <- data.frame(model.matrix(~mari - 1))
tmp_educ <- data.frame(model.matrix(~ed - 1))
dat$default <- as.numeric(as.factor(dat$default)) - 1
dat$housing <- as.numeric(as.factor(dat$housing)) - 1
dat$loan <- as.numeric(as.factor(dat$loan)) - 1
tmp_contact <- data.frame(model.matrix(~cntct - 1))
tmp_month <- data.frame(model.matrix(~m - 1))
tmp_poutcome <- data.frame(model.matrix(~pout - 1))
dat$deposit <- as.numeric(as.factor(dat$deposit)) - 1

pdaysnew <- ifelse(dat$pdays != -1, dat$pdays, 0)
nocntct <- ifelse(dat$pdays == -1, 1, 0)

names(dat)
dat1 <- cbind(dat[,c(17,1,5:8,10,12:13,15)],
              tmp_job[,1:11], tmp_marit[,1:2], tmp_educ[,1:3],
              tmp_contact[,1:2], tmp_month[,1:11], 
              tmp_poutcome[,1:3],
              data.frame(pdaysnew, nocntct))
names(dat1)

rm(tmp_job, tmp_marit,tmp_contact,tmp_month,tmp_poutcome,tmp_educ)
rm(jb, mari, ed, cntct, m, pout, nocntct, pdaysnew)

# It does not make sense to use the pdays variable as it is.
# This is because logistic regression works only on [0,1] level with sigmoid function
# dependent variables and not on [-1,1] level dependent variables.
# Also if we use cross entropy as the loss function we cannot have the 
# dependent variable belonging to the level [-1,1]
# Also perhaps a new column which denotes if the person has been contacted earlier
# could be made with the pdays variable.


#
#
#  Part 2.  
#
#

library("corrplot")

sort(abs(cor(dat1)[1,]))
# The 3 variables that are the most correlated with the deposit variable are
# duration, poutsuccess and nocntct.
# It makes success that poutsuccess is important because it makes sense that 
# the success in the previous marketing campaign is correlated to a deposit this
# time too.
# Also if the last contact duration is high it makes sense that the correlation
# is higher.
# It is natural that nocntct is among the most correlated variables.
# This is because if the person has been contacted earlier there is a higher
# chance that he or she will not respond.

succ <- subset(dat1, dat1$deposit == 1)
fail <- subset(dat1, dat1$deposit == 0)
set.seed(112233)


train.succ <- sample(1:nrow(succ),3000)
train.fail <- sample(1:nrow(fail),3000)

dat.train <- rbind(succ[train.succ,],fail[train.fail,])
str(dat.train)
dim(dat.train)
loan.count.train <- table(dat.train$deposit)
loan.count.train


newfail <- fail[-train.fail,]
test.fail <- newfail[sample(1:nrow(newfail),2289),]
dat.test <- rbind(succ[-train.succ,],test.fail)


#
#
#  Part 3.  
#
#

logreg <- glm(deposit ~ ., data = dat.train, family = "binomial")
summary(logreg)
# The variables that are significant are housing, loan, duration, campaign,
# marimarried,edprimary, cntctcellular,cntcttelephone,mapr, maug,mfeb,mjan,
# mjul,mjun,mmay,mnov and poutsuccess.

yhat.test <- predict(logreg, dat.test)

yhat.test.class <- ifelse(yhat.test > 0.5, 1, 0)
yhat.test.class[1:20]

tab.lr1.test <- table(dat.test$deposit, 
                      yhat.test.class, 
                      dnn = c("Actual","Predicted"))
tab.lr1.test

#Predicted
#Actual    0    1
#0 2015  274
#1  600 1689

mean(yhat.test.class != dat.test$deposit)
# 0.1909131


#
#
#  Part 4.  
#
#



library(dplyr)

dat.train = dat.train %>%
  select(deposit,housing, loan, duration, campaign,marimarried,edprimary, cntctcellular,cntcttelephone,mapr, maug,mfeb,mjan,mjul,mjun,mmay,mnov,poutsuccess)

dat.test = dat.test %>%
  select(deposit,housing, loan, duration, campaign,marimarried,edprimary, cntctcellular,cntcttelephone,mapr, maug,mfeb,mjan,mjul,mjun,mmay,mnov,poutsuccess)

# I chose these variables out of all those because in the all in logistic regression they were the
# ones which were significant.

logreg <- glm(deposit ~ ., data = dat.train, family = "binomial")
summary(logreg)

yhat.test <- predict(logreg, dat.test)

yhat.test.class[1:20]
yhat.test.class <- ifelse(yhat.test > 0.5, 1, 0)
yhat.test.class[1:20]

tab.lr1.test <- table(dat.test$deposit, 
                      yhat.test.class, 
                      dnn = c("Actual","Predicted"))
tab.lr1.test

#Predicted
#Actual    0    1
#0 2078  211
#1  642 1647

mean(yhat.test.class != dat.test$deposit)
# 0.1863259


#
#
#  Part 5.  
#
#

library(MASS)

lda.fit <- lda(deposit ~ ., data = dat.train)
lda.fit

lda.pred <- predict(lda.fit, dat.test)
lda.pred
lda.test.class <- lda.pred$class
lda.test.class

tab.lda <- table(dat.test$deposit, lda.test.class,
                 dnn = c("Actual", "Predicted"))
tab.lda

#Predicted
#Actual    0    1
#0 2003  286
#1  505 1784

err.lda <- mean(dat.test$deposit != lda.test.class)
err.lda

# 0.1727829


#
#
#  Part 6.  
#
#

library(e1071)

nb.fit <- naiveBayes(deposit ~ ., data = dat.train)
nb.fit



nb.class <- predict(nb.fit, newdata = dat.test)
nb.class

tab.nb <- table(dat.test$deposit, nb.class,
                dnn = c("Actual", "Predicted"))
tab.nb

#Predicted
#Actual    0    1
#0 1969  320
#1  900 1389


err.nb <- mean(dat.test$deposit != nb.class)
err.nb
# 0.2664919


#
#
#  Part 7.  
#
#


library(class)

dat.train.x <- dat.train[,2:18]
dat.train.y <- dat.train[,1]
dat.train.y
dat.test.x <- dat.test[,2:18]
dat.test.y <- dat.test[,1]
dat.test.y

out1 <- knn(dat.train.x, dat.test.x, dat.train.y, k=10)
out1[1:25]


tab.knn1 <- table(dat.test.y, out1,
                  dnn = c("Actual", "Predicted"))
tab.knn1

#Predicted ----- k=1
#Actual    0    1
#0 1628  661
#1  578 1711


#Predicted ------ k=2
#Actual    0    1
#0 1567  722
#1  560 1729

#Predicted ------ k=3
#Actual    0    1
#0 1568  721
#1  499 1790

#Predicted ------ k=5
#Actual    0    1
#0 1579  710
#1  469 1820

#Predicted ------ k=10
#Actual    0    1
#0 1580  709
#1  453 1836



knn1.err <- mean(dat.test.y != out1)
knn1.err
# 0.2706422 ----- k=1
# 0.2800349 ------ k=2
# 0.2664919 ------- k=3
# 0.257536 ------ k=5
# 0.2538226 ------ k=10

# The value of k which fits the best is k=10


#
#
#  Part 8.  
#
#

library(tree)


dat.train[,1] <- as.factor(dat.train[,1])
dat.test[,1] <- as.factor(dat.test[,1])

tree1 <- tree(deposit~., data = dat.train)
summary(tree1)

plot(tree1)
text(tree1, pretty = 0)

tree.pred.tst <- predict(tree1, dat.test, type = "class")
table(dat.test$deposit, tree.pred.tst,
      dnn = c("Actual", "Predicted"))

#Predicted ------ k=10
#Actual    0    1
#0 1816  473
#1  484 1805


mean(dat.test$deposit != tree.pred.tst)
# 0.2090433


#
#
#  Part 9.  
#
#

prune1 <- prune.misclass(tree1)
names(prune1)

plot(prune1)
plot(prune1$size, prune1$dev, xlab = "Size of Tree",
     ylab = "Deviation")


prune.tree1 <- prune.misclass(tree1, best = 3)
summary(prune.tree1)
prune.tree1
plot(prune.tree1)
text(prune.tree1, pretty = 0)

tree1.pred <- predict(tree1, dat.test, type = "class")
table(dat.test$deposit, tree1.pred,
      dnn = c("Actual", "Predicted"))
#Predicted
#Actual    0    1
#0 1816  473
#1  484 1805

mean(dat.test$deposit != tree1.pred)
#0.2090433
pt1.pred <- predict(prune.tree1, dat.test, type = "class")
table(dat.test$deposit, pt1.pred,
      dnn = c("Actual", "Predicted"))
#Predicted
#Actual    0    1
#0 1816  473
#1  484 1805
mean(dat.test$deposit != pt1.pred)
#0.2090433
# 0.2671472

# I chose the size of the subtree to be 4 and it gave the same test error rate.
# Therefore it is a very good option.
# I then chose the size to be 3 and it gave an error rate of 0.26715
# This is only slightly worse than the one for the unpruned tree.
# The usage of a pruned tree makes explaining it very easy.
# We can say that if the duration is lesser than 202.5 and the poutsuccess is
# lesser than 0.5 the deposit will be 0 and otherwise it will be 1.


#
#
#  Part 10.  
#
#

# The technique which was the best was the LDA. It is followed by Logistic regression.
# The next best technique was decision tree, followed by naive bayes. 
# We know this because the error rate for LDA was the best followed by the others
# in that order. 
# The LDA performed the best because the LDA works by creating a function that
# discriminates between the classes.It divides the data so that the two groups have means 
# that are separated as far as possible and such that the standard deviations within the 
# groups are as small as possible and that is why it performed the best.

# The bank management should know these things:-

# a) The duration is the most important factor. The marketing team should try to make
# the last contact duration as long as possible.More than 200 seconds.

# b) Also the bank marketing team should try its very best to contact people who
# the previous campaign had success with.

# c) Also it looks like the campaign variable is negatively correlated. So perhaps,
# it will be better if the marketing team does not pester the people too much too.

# d) Also perhaps the marketing team should not concentrate on those people who have a 
# housing loan.






