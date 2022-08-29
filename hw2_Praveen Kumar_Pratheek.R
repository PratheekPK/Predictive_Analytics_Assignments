#
#  Answers to Homework Assignment 2
#  BA 288 Winter 2022
#  Murphy 1/22/22 Draft
#
#
#  Read in dataset 
#
list.files(path = ".")
dat <- read.csv("hw2_credit_card_balance_new.csv")
str(dat)
head(dat)
#
#
#  Part a.  Read and prepare the dataset
#
dat$Married
dat$Married <- as.numeric(as.factor(dat$Married)) - 1
dat$Married

dat$Ethnicity
Eth <- as.factor(dat$Ethnicity)
Eth
tempEth <- data.frame(model.matrix(~Eth - 1))
tempEth

newdat <- cbind(dat, tempEth)
newdat
names(newdat)

newdat$Student
Student <- as.numeric(as.factor(newdat$Student)) - 1
Student
newdat$Student <- Student
newdat$Student


newdat$Gender
Gender <- as.numeric(as.factor(newdat$Gender)) - 1
Gender
newdat$Gender <- Gender
newdat$Gender

dat.final <- newdat[,c(2:10, 12:17)]
dat.final

rm(newdat, Gender, Student, Eth)


#
#
#  Part b.  Correlation between all independent variables and the response, Balance
#

cor(dat.final)[,10]

# Income        Limit       Rating    Num_Cards          Age       Yrs_Ed 
#0.446919280  0.855190700  0.857339800  0.087810661  0.006591191 -0.050196896 
#Gender      Student      Married      Balance         Eth1         Eth2 
#0.012684800  0.234686398 -0.001521574  1.000000000  0.008986931  0.030224274 
#Eth3         Eth4         Eth5 
#0.012950935 -0.055304772 -0.036727061

# The top 3 correlated variables are :- Rating, Limit and Income

# It makes a lot of sense that Rating, Limit and Income are correlated with
# the Balance independent variable. If the rating and limit is high, the balance
# will be high because the person is obviously someone who has more money.
# Also if the person has more income it makes sense that he or she has
# a higher balance. All these 3 variables are positively correlated with the
# balance. This means that higher values of all these variables increase the
# value of the balance.


#
#
#  Part c. Correlation between 3 independent variables and the response, Balance
#

reg.1 <- lm(Balance ~ Income + Limit + Rating, data = dat.final)
reg.1
summary(reg.1)

# The coefficient of determination value is Multiple R-squared:  0.8657
# This means that 86.57% of the variation in Balance is predictable by the 
# Income, Limit and Rating variables.
# The income and rating variables are extremely significant because the p
# values are extremely close to 0. On the other hand the limit is less
# significant but significant at a level of 0.05

# With an decrease in the value of the income by -7.55057 units  
# the value of the balance increases by 1
# With an increase in the value of the limit by 0.08271 units  
# the value of the balance increases by 1
# With an increase in the value of the rating by 2.67475 units  
# the value of the balance increases by 1

# The fact that the income coefficient is negative makes no sense and suggests
# that there might be some correlation between the independent variables.
# However, the R squared value is extremely good and the residual standard error
# is 168.4 which is extremely less. So on the whole from a prediction point of
# view is extremely good but from an explainability point of view is not good.

#
#
#  Part d. Correlation between all independent variables and the response, Balance
#

reg.2 <- lm(Balance ~ ., data = dat.final)

summary(reg.2)

# The income, limit, rating, num_cards, gender, student, eth1, eth2, eth3 and eth4
# variables are significant
# The model fit is extremely good because the R squared value is 0.9426 and the
# residual standard error is 109.7

anova(reg.1,reg.2)

# The p value is extremely low. So we can reject Ho. Therefore the big model is
# better. Therefore to build a model to predict Balance we need to use all the
# independent variables


#
#
#  Part e. linear regression model that is “best” for predicting Balance (“model best”).
#
# 

reg.3 <- lm(Balance ~ Income + Limit + Rating + Num_Cards + Gender + Student + Eth1 + Eth2 + Eth3 + Eth4, data = dat.final)
summary(reg.3)
summary(reg.2)

# The reg.3 model seems to be the best option. It has a r squared value which is 
# as good as the all-model. However, it does not contain the irrelevant variables.
# It does not have the age, yrs_ed and married variables. The residual standard
# error also is the same as the all-in model.

yhat.6 <- predict(reg.3, dat.final[250,],interval="predict")
yhat.6

# The 95% prediction interval forecast is as follows
#      fit      lwr      upr
#250 639.7683 422.6494 856.8872


#
#
#  Part f. Interaction model and Polynomial model
#
# 

# Polynomial regression model

reg.4 <- lm(Balance ~ Income + I(Rating^2) + Limit + Rating + Num_Cards + Gender + Student + Eth1 + Eth2 + Eth3 + Eth4, data = dat.final)
summary(reg.4)

# This makes sense because while observing the dataset we can see that the 
# rating variable increases in an exponential fashion. The significance code is
# 3 stars.


# Interaction model

reg.5 <- lm(Balance ~ Income + I(Rating^2) + Gender*Eth4 + Limit + Rating + Num_Cards + Gender + Student + Eth1 + Eth2 + Eth3 + Eth4, data = dat.final)
summary(reg.5)

# This makes sense because there might be some specific combination of gender and
# ethnicity who have around the same level of Balance. The significance code is
# 2 stars.


#
#
#  Part g. Train and test dataset
#
# 


set.seed(975246)
rows.train <- sample(1:450,300)

dat2.train <- dat.final[rows.train,]
dat2.test <- dat.final[-rows.train,]

reg.6 <- lm(Balance ~ Income + Limit + Rating + Num_Cards + Gender + Student + Eth1 + Eth2 + Eth3 + Eth4, data = dat2.train)
summary(reg.6)

RSS.6 <- sum(reg.6$residuals^2)
RSS.6
# 3271819
MSE.6 <- RSS.6/300
MSE.6
# 10906.06
RMSE.6 <- sqrt(MSE.6)
RMSE.6
# 104.4321

# Surprisingly the RMSE value is slightly lower than the value for the model best.(106.4 vs 109.7)
# The coefficient of determination value on the other hand remains around the same. 


#
#
#  Part h. Test dataset
#
# 

yhat.6.tst <- predict(reg.6, dat2.test)

resid.3.tst <- dat2.test$Balance - yhat.6.tst
RSS.3.tst <- sum(resid.3.tst^2)
RSS.3.tst
# 2129635
MSE.3.tst <- RSS.3.tst/88
MSE.3.tst
# 24200.39
RMSE.3.tst <- sqrt(MSE.3.tst)
RMSE.3.tst
# 155.5648

# The RMSE value for the test dataset is substantially higher than the RMSE value 
# for the training dataset. This shows that the model has perhaps overfitted on
# the training dataset. The model shows extremely low bias on the training dataset
# but perhaps that has caused the variance to be high on useen or new data.
# There is thus a tradeoff between bias and variance.


#
#
#  Part i. Model all
#
# 



yhat.6.tst <- predict(reg.3, dat2.test)

resid.3.tst <- dat2.test$Balance - yhat.6.tst
RSS.3.tst <- sum(resid.3.tst^2)
RSS.3.tst
# 1931581
MSE.3.tst <- RSS.3.tst/88
MSE.3.tst
# 21949.78
RMSE.3.tst <- sqrt(MSE.3.tst)
RMSE.3.tst
# 148.1546

# The results do not differ substantially from the results we got in section h.
# The RMSE value has decreased by a bit but it is nothing substantially. The 
# model still overfits on the training dataset.



#
#
#  Part j. Insights
#
#

# It looks as if the the income, limit, number of cards, student and the ethnicity
# variables are significant variables for predicting the balance. It looks like
# the number of cards, student and the ethnicities are positively correlated with
# the balance level. On the other hand the income and being female are negatively
# correlated.
# The bank can thus decide to target students and certain ethnicities denoted in the linear
# regression model by eth1, eth2 and eth3.
# The predictions are actually extremely accurate and differ with a RMSE of around
# 150 dollars on the test dataset.
# Additional information which could help include location, rent, number of kids.









