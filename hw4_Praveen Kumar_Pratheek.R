#
#  Answers to Homework Assignment 4
#  BA 288 Winter 2022
#  Murphy 2/5/22 Draft
#
#
#  Read in dataset 
#
list.files(path = ".")
dat <- read.csv("hw4_home_starts.csv")
str(dat)
head(dat,20)

#
#
#  Part a.  Read and prepare the dataset
#

dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
dat$Month <- as.factor(dat$Month)
dat$Quarter <- as.factor(dat$Quarter)

dat$timetr <- 1:756 
dat$timetrsq <- dat$timetr^2
str(dat)

# The candidates for seasonal variables are Month and Quarter.
# This is because seasonal variables repeat in a specific pattern with time.
# Both quarter and month repeat with a specific pattern and therefore
# are seasonal variables.

#
#
#  Part b.  2.	Run a regression model to predict “Starts”  with the time variable alone.  
# Try adding the squared time trend variable to capture more of the signal.   
# Try cubic and quartic (sp?) terms in time also.  
# Comment on the quality of these models.  Is there a significant trend in the data set?  
# Compute the RMSE for the quadratic trend model.  
# Plot the actual housing starts and the fitted values on the same graph.
#


reg1 <- lm(Starts ~ timetr, data = dat)
summary(reg1)

reg2 <- lm(Starts ~ timetr + timetrsq, data = dat)
summary(reg2)

reg3 <- lm(Starts ~ timetr + timetrsq + timetr^3 + timetr^4, data = dat)
summary(reg3)

# The quality of the models is quite bad actually. The r squared value is 9.8% for
# the squared, cubic and quartic models which is better than the r squared value 
# for the time variable alone which is 6.7%
# There does not seem to be a significant trend ass the coefficient for timetr is
# -0.045
# The RMSE for the quadratic trend model is 36.09

plot(dat$timetr, dat$Starts)
lines(dat$timetr, dat$Starts, type = "l")
abline(reg1, col = "purple")


#
#
#  Part c. Repeat the previous question, using regression to model “Start” 
# by adding the “season” variables to the quadratic trend model fit above.  
# Compare this result to the earlier model.  Which is preferred and why?  
# Compute the RMSE.  
# Plot the actual housing starts and the fitted values on the same graph. 
# 
#

reg4 <- lm(Starts ~ timetr + timetrsq + timetr^3 + timetr^4 +  Quarter, data = dat)
summary(reg4)

# The R squared value increased from 9.8% to 26.4% where as the residual
# standard error decreased from 36.09 to 32.67
# Therefore this model is preferred to the previous one as the R squared value 
# has increased and therefore a higher amount of variation in the Starts variable
# is explained.
# The RMSE is 32.67 for this model.

plot(dat$timetr, dat$Starts)
lines(dat$timetr, dat$Starts, type = "l")
abline(reg4, col = "purple")


#
#
#  Part d. 4.	Repeat the previous question by adding the “month” variables 
# to the model of the previous question to predict “Starts”.  
# Remedy any errors that occur.  Why do these errors occur?  
# Once the errors are removed compare this model to the result of the earlier models.  
# Which is preferred and why?  
# Compute the RMSE. 
# Plot the actual housing starts and the fitted values on the same graph. 
# 
#

reg5 <- lm(Starts ~ timetr + timetrsq + timetr^3 + timetr^4 +  Quarter + Month, data = dat)
summary(reg5)

# No errors happened. Only the Month6, Month9 and Month12 became NAs.This is 
# because these variables are linearly related to the other independent variables.
# The R squared value increased from 26.4% to 34.04% where as the residual
# standard error decreased from 32.67 to 31.1
# Therefore this model is preferred to the previous one as the R squared value 
# has increased and therefore a higher amount of variation in the Starts variable
# is explained.
# The RMSE is 31.1

plot(dat$timetr, dat$Starts)
lines(dat$timetr, dat$Starts, type = "l")
abline(reg5, col = "purple")


#
#
#  Part e. 
# 
#

library(tsibble)
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
dat1 <- mutate(dat, Month = yearmonth(Date))
HSdat <- as_tsibble(dat1[,c(2,4)], index = Month)
str(HSdat)
library(fpp3)
autoplot(HSdat)
# There seems to be a yearly cycle in the signal and also a seasonal cycle.

gg_season(HSdat, polar = TRUE) +
  ggtitle("Starts by Month") +
  ylab("Starting prices") +
  xlab("Year")


gg_lag(HSdat, Starts)
gg_lag(HSdat, Starts, lags = 1:12)

# There is a significant autocorrelation in the plot. This is because the 
# colored lines are closely hugging the 45 degree line.Therefore it shows
# 12 month seasonality.

ACF(HSdat, Starts, lag_max = 20)
autoplot(ACF(HSdat, Starts, lag_max = 20))

# There is a lot of correlation according to the autocorrelation graph.


#
#
#  Part f. 
# 
#


mod_drift <- model(HSdat, RW(Starts ~ drift()))
forc_drift <- forecast(mod_drift, h = "28 months")
autoplot(forc_drift, HSdat)

mod_snaive <- model(HSdat, SNAIVE(Starts))
forc_snaive <- forecast(mod_snaive, h = "28 months")
autoplot(forc_snaive, HSdat)

mod_t <- model(HSdat, 
               Mean = MEAN(Starts),
               Naive = NAIVE(Starts),
               Seasonal_Naive = SNAIVE(Starts),
               Drift = RW(Starts))
forc_t <- forecast(mod_t, h = 28)
autoplot(forc_t, HSdat, level = NULL)

res_drift <- augment(mod_drift)$.resid
MSE_drift <- mean(res_drift^2, na.rm = TRUE)
RMSE_drift <- MSE_drift^0.5
RMSE_drift
#
res_snaive <- augment(mod_snaive)$.resid
MSE_snaive <- mean(res_snaive^2, na.rm = TRUE)
RMSE_snaive <- MSE_snaive^0.5
RMSE_snaive

# The RMSE of the drift is much lesser than the RMSE of the snaive model.



#
#
#  Part g. 
# 
#


ASts1_5 <- mutate(HSdat, 
                MA5 = slider::slide_dbl(Starts,mean,.before = 2, 
                                        .after = 2,complete = TRUE))

ASts1_9 <- mutate(HSdat, 
                  MA5 = slider::slide_dbl(Starts,mean,.before = 4, 
                                          .after = 4,complete = TRUE))

ASts1_11 <- mutate(HSdat, 
                  MA5 = slider::slide_dbl(Starts,mean,.before = 5, 
                                          .after = 5,complete = TRUE))
ASts1_17 <- mutate(HSdat, 
                  MA5 = slider::slide_dbl(Starts,mean,.before = 8, 
                                          .after = 8,complete = TRUE))
install.packages("patchwork")
library(patchwork)

autoplot(ASts1_5) +
  autoplot(ASts1_9) +
  autoplot(ASts1_11) +
  autoplot(ASts1_17)


ASts1$residuals = (ASts1$Starts - ASts1$MA5)^2

RMSE_MA <- (mean(ASts1$residuals))^0.5
RMSE_MA
# 5 ma - 10.693
# 9 ma - 17.723
# 11 ma - 20.188
# 17 ma - 24.854

# The RMSE for the best model which is MA5 is 10.693

# The smaller range models will fit better because the smaller range models are averaged over a smaller time period.
# Because of this, the present component will be higher and the effect of the other components
# will be less.


#
#
#  Part h. 
# 
#


mod_cda <- model(HSdat, classical_decomposition(Starts, 
                                               type = "additive"))
comp_cda <- components(mod_cda)
autoplot(comp_cda)



mod_cdm <- model(HSdat, classical_decomposition(Starts, 
                                               type = "multiplicative"))

mod_cdm
comp_cdm <- components(mod_cdm)
autoplot(comp_cdm)
comp_cdm

# From these 2 graphs we can see that the trend and the seasonality is the same for
# both.The trend is indefinite. There is a strong seasonal component for each year
# though.

MSE_cda <- mean(comp_cda$random^2, na.rm = TRUE)
RMSE_cda <- MSE_cda^0.5
RMSE_cda
MSE_cdm <- mean(comp_cdm$random^2, na.rm = TRUE)
RMSE_cdm <- MSE_cdm^0.5
RMSE_cdm


#
#
#  Part i. 
# 
#

dcmp <- HSdat
mod1 <- model(dcmp, stl = STL(Starts,t.window = NULL))
# I used the value of t.window = NUll
components(mod1)
comp1 <- components(mod1)
autoplot(comp1)


MSE_stl <- lapply((comp1[6])^2, mean, na.rm = TRUE)
RMSE_stl <- sqrt(MSE_stl$remainder)

RMSE_stl


#
#
#  Part j.10.	Compare all RMSEs computed in this homework assignment.  
# Which “model” had the lowest RMSE?  
# How would you feel about the “best” model given the comparison with other models?  
# On its own, that is in absolute terms?  
# Will this “best” provide effective forecasts in the future?  
# Summarize in a few sentences the signals detected in the housing 
# starts data based on the work performed in this assignment.    
# 
#

list_of_RMSEs = list(RMSE_cda,RMSE_cdm,RMSE_drift,RMSE_MA,RMSE_mean,RMSE_naive,RMSE_snaive,RMSE_stl)

plot(list_of_RMSEs)

barplot(unlist(list_of_RMSEs),names.arg=colnames(list_of_RMSEs))


# The model which has the lowest RMSE is the multiplicative classical decomposition.
# It seems to be the best performing model by far and has an extremely low RMSE
# value.

# In absolute terms, the model seems to be a decent predictor and is the best 
# amongst the models used.
# It will be able to provide a good forecasts in the future and is the best among the models
# tried out.


# According to the data given, there is a very definite seasonal component. This seasonal
# component varies by year and can be very well captured. There is also a trend component
# which can be seen in the data. This trend is neither constantly increasing or
# constantly decreasing. It has its ups and downs.



















