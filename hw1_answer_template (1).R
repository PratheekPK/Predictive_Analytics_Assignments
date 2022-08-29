#
#  Answers to Homework Assignment 1
#  BA 288 Winter 2022
#  Murphy 1/7/22 Draft
#
#
#  Read in Universal Bank Data 
#
list.files(path = ".")
dat <- read.csv("hw1_universal_bank.csv")
str(dat)
#
#
#  Part a.  Graphical descriptive statistics
#

tab1 <- table(dat$Acpt_Offer)
barplot(tab1)
barplot(tab1, ylab = "Accepted offer", 
        xlab = "Count", horiz = T, col = c("Green","Violet"),
        main = "Accepted offer vs not accepted offer")

tab2 <- table(dat$Education)
pie(tab2, names(tab2), main = "Education levels")

hist(dat$Income)
hist(dat$Income, main = "Distribution of Income",
     xlab = "Income")

boxplot(dat$Work_Exp,main = "Boxplot of work experience",
        xlab = "Work experience")
#
#
#  Part b.  Conditional boxplot of Credit Card Debt on Acept Offer
#

#(add answers)
boxplot(Crdt_Crd_Avg~Acpt_Offer, data = dat, main = "boxplot of Credit Card Debt on Accept Offer",
        horizontal = F)
boxplot(Income~Education, data = dat, main = "Boxplot of Income on Education",
        horizontal = F)
#
#  Part c.  Scatter plots of Credit Card Debt on Income and 
#    Accept Offer on Credit Card Debt
#

plot(dat$Income,dat$Crdt_Crd_Avg,xlab="Income",ylab="Average credit card spending")
abline(lm(dat$Crdt_Crd_Avg~dat$Income))

#This informs us that the higher the income of a person is, the higher they will spend on their credit card.

plot(dat$Income,dat$Acpt_Offer,xlab="Income",ylab="Accept Offer")
abline(lm(dat$Acpt_Offer~dat$Income))
#This chart informs us that the people with lower income (below 60 approx) will not accept the offer. In general if the income is higher people will accept the offer.


#
#  Part d.  Three unique insights regarding the Universal Bank 
#      data 
#
#

# Insight 1 :- the number of accepted offers is much less than the number of
# not accepted offers. Therefore it is an imbalanced dataset.
# Insight 2 :- the distribution of income and family size are both positively skewed
# Insight 3 :- The people who have accepted the offer on average have a much higher credit card
# spend average than those who have not accepted the offer.

#
#  Part e.descriptive statistics, e.g., means, medians, standard deviations, etc., for all variables in the data set.  
#


apply(dat, 2, mean)
# Looks like the average age is quite high at around 45 and the work experience is around 20.
apply(dat, 2, sd)
apply(dat, 2, median)
# We can also say that the Income family size distribution is positively skewed. This is because
# the mean of the income and family size is higher than the median of the income and family size.

#
#  Part f. Pearson correlation for all pairs variables
#
#

cor(dat)
cor(dat)[15,]
# Income, Credit Card Average and CD_Account are the ones with the highest correlation values with Accept Offer.
# It is extremely surprising to me that people who have the highest income are the ones who are
# most likely to take a loan. Ditto for credit card average spend.I also find it less
# surprising that people who have a CD account are more likely to take a loan. This is because
# their money is tied down in the bank and they do need liquid money to spend.


#
#  Part g. simple linear regression that predicts monthly average credit card spending 
# (“Crdt_Crd_Avg”) as a function of income
#

reg <- lm(Crdt_Crd_Avg ~ Income, data = dat)
summary(reg)
# The estimated regression model is Crdt_Crd_Avg = 0.1103670 + Income*0.0247278
# The adjusted R-squared value is 0.413. This means that only
# 41.3 percent of the variation in the average credit card spending 
# is explained by the income.
# This means that for a unit increase in the income, the average credit
# card spending increases by 0.02473.
# Yes the variable is significant because the P value is lesser than
# 2e-16.
# If a variable is significant, it means that there is an insignificant chance 
# that the value of the variable is 0 or that the variable has no effect
# on the value of the dependent variable.

#
#  Part h. simple linear regression that predicts which 
# customers will accept the bank’s offer of the personal 
# loan product (“Acpt_Offer”)  as a function of income
#

reg2 <- lm(Acpt_Offer ~ Income, data = dat)
summary(reg2)
# This model has an adjusted R squared value of 0.2672
# This means that only 26.7 percent of the variation in the Accept value spending 
# is explained by the income.This means the model does not fit well.
# This means that for an unit increase in the income the value of the
# accept offer goes up by 0.003361
# This is a linear regression that we have run. We should
# have run a logistic regression for it to be useful.The explainability
# suffers because this is a linear regression model and therefore
# this is not a useful model from a usefullness point of view or
# a prediction point of view.

#
#  Part i. Provide and interpret a 99% prediction 
# interval estimate for the dependent (“Y”) variable.
#
# Model 1

newdat <- data.frame(Income = 75)
predict(reg, newdat, interval = "prediction", level = 0.99)

#       fit       lwr     upr
# 1 1.964954 -1.581923 5.51183
# It looks like the average credit card spending can vary between
# -1.58 to 5.51 for a prediction interval of 99%. This estimate is 
# not useful for decision makers because the difference between the 
# upper limit and the lower limit is extremely high.

# Model 2

newdat <- data.frame(Income = 75)
predict(reg2, newdat, interval = "prediction", level = 0.99)

#         fit        lwr       upr
# 1 0.1042577 -0.5650005 0.7735159
# It looks like the accept offer can vary between
# -0.565 to 0.774 for a prediction interval of 99%
# This makes no sense at all as there are only 2 values of accept offer.
# 

#
#  Part j. predict whether a customer would accept 
# the bank’s product offer
#
# The 2 statistic modeling methods that can be used are
# 1) logistic regression
# 2) support vector classification
# 3) random forest classifier
# They are all classification models and acpt_offer has only 2
# acceptable values and therefore classification models 
# like these should be used.