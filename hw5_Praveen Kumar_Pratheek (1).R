#
#  Answers to Homework Assignment 5
#  BA 288 Winter 2022
#  Murphy 2/18/22 Draft
#
#
#  Read in dataset 
#
list.files(path = ".")
dat <- read.csv("hw5_bike_share_day.csv")
str(dat)
head(dat,20)

#
#
#  Part 1.  
#
library(fpp3)
cnts <- ts(dat[,14],frequency = 7)
cntts <- as_tsibble(cnts)
names(cntts)[2] <- "count"
str(cntts)


fit_bike_SES_25 <- model(cntts, ETS(count ~ error("A") + 
                                   trend("N", alpha = 0.25) + 
                                   season("N")))
report(fit_bike_SES_25)
accuracy(fit_bike_SES_25)



fit_bike_SES_75 <- model(cntts, ETS(count ~ error("A") + 
                                      trend("N", alpha = 0.75) + 
                                      season("N")))
report(fit_bike_SES_75)
accuracy(fit_bike_SES_75)


fit_bike_SES_null <- model(cntts, ETS(count ~ error("A") + 
                                      trend("N") + 
                                      season("N")))
report(fit_bike_SES_null)
accuracy(fit_bike_SES_null)


fit_bike_tr <- model(cntts, 
                     Mean = MEAN(count),
                     Naive = NAIVE(count),
                     Drift = NAIVE(count ~ drift()),
                     SNaive = SNAIVE(count)
)
report(fit_bike_tr)
accuracy(fit_bike_tr)

# It looks like the best performing model is the one with alpha = 0.25 and the parameters optimised one,
# followed by the one with alpha = 0.75 and the worst performing model among the three is the naive one.

#
#
#  Part 2
#

fit_bike_Holt_additive_trend <- model(cntts, 
                          ETS(count ~ error("A") 
                              + trend("A") 
                              + season("N"))
)

report(fit_bike_Holt_additive_trend)
accuracy(fit_bike_Holt_additive_trend)


fit_bike_Holt_multiplicative_trend <- model(cntts, 
                                      ETS(count ~ error("A") 
                                          + trend("M") 
                                          + season("N"))
)

report(fit_bike_Holt_multiplicative_trend)
accuracy(fit_bike_Holt_multiplicative_trend)

# The Holt multiplicative model performs slightly better than the alpha = 0.25 and parameters optimised model 
# and also slightly better than the Holt additive model.


#
#
#  Part 3
#

fit_bike_HWa <- model(cntts, 
                         ETS(count ~ error("A") 
                             + trend("A") 
                             + season("A"))
)


accuracy(fit_bike_HWa)


fit_bike_HWm <- model(cntts, 
                         ETS(count ~ error("A") 
                             + trend("A") 
                             + season("M"))
)
accuracy(fit_bike_HWm)

fit_bike_HWd <- model(cntts, 
                           ETS(count ~ error("A") 
                               + trend("Ad") 
                               + season("M"))
)
accuracy(fit_bike_HWd)

# The Holt Winters damped multiplicative method performs the best followed by the
# HW additive method and the HW multiplicative method according to RMSE. All the models
# perform much better than the models in questions 1 and 2. No, the results are not
# surprising at all because the seasonality part has been added here. Therefore the 
# seasonality aspects are being captured and it is more accurate.

#
#
#  Part 4
#


forc_bike_HWd <- forecast(fit_bike_HWd, h = 28)
forc_bike_HWd
str(forc_bike_HWd)
autoplot(forc_bike_HWd, cntts)

forc_bike_Holt_multiplicative_trend <- forecast(fit_bike_Holt_multiplicative_trend, h = 28)
forc_bike_Holt_multiplicative_trend
str(forc_bike_Holt_multiplicative_trend)
autoplot(forc_bike_Holt_multiplicative_trend, cntts)


forc_bike_SES_null <- forecast(fit_bike_SES_null, h = 28)
forc_bike_SES_null
str(forc_bike_SES_null)
autoplot(forc_bike_SES_null, cntts)


#
#
#  Part 5
#

data()
JJ <- JohnsonJohnson
JJ <- as_tsibble(JJ)

str(JJ)
length(JJ)
# There are 84 rows ranging from Q1 1960 to Q4 1980.
# The periodicity of the data is one quarter.

fit_JJ_SES <- model(JJ, 
                         ETS(value ~ error("A") + trend("A") + season("A"))
)
accuracy(fit_JJ_SES)

report(fit_JJ_SES)

aug_SES <- augment(fit_JJ_SES)
autoplot(aug_SES, value) +
  autolayer(aug_SES,.fitted, colour = "Red") +
  autolayer(aug_SES,.resid, colour = "Green")


#
#
#  Part 6
#

fit_JJ_best <- model(JJ, 
                    ETS(value ~ error("M") + trend("M") + season("M"))
)
accuracy(fit_JJ_best)

report(fit_JJ_best)

#The best model is the ETS MMM model which has an AIC of 164.0222 and an Rmse of 0.423
# It is the best because the components of the model are related to each other in a
# multiplicative fashion.


aug_SES_best <- augment(fit_JJ_best)
autoplot(aug_SES_best, value) +
  autolayer(aug_SES_best,.fitted, colour = "Red") +
  autolayer(aug_SES_best,.resid, colour = "Green")


forc_JJ_SES_best <- forecast(fit_JJ_best, h = 12)
forc_JJ_SES_best
str(forc_JJ_SES_best)
autoplot(forc_JJ_SES_best, JJ) 


#
#
#  Part 7
#

# Information criteria can be used for model selection. AIC, BIC and AICc could
# be used in this regard. By default it uses the AICc to select an 
# appropriate model, although other information criteria can be selected.


#
#
#  Part 8
#

usemp <- us_employment
usemp <- filter(usemp, Title == "Total Private")
usemp <- usemp[,c(1,4)]
autoplot(usemp, Employed) 

fit_USEmp_best <- model(usemp, 
                     ETS(Employed ~ error("A") + trend("Ad") + season("M"))
)
accuracy(fit_USEmp_best)

report(fit_USEmp_best)

# Smoothing parameters:
# alpha = 0.8679675 
# beta  = 0.1800413 
# gamma = 0.130548 
# phi   = 0.9545302 

aug_USEmp_best <- augment(fit_USEmp_best)
autoplot(aug_USEmp_best, Employed) +
  autolayer(aug_USEmp_best,.fitted, colour = "Red") +
  autolayer(aug_USEmp_best,.resid, colour = "Green")


forc_USEmp_best <- forecast(fit_USEmp_best, h = 60)
forc_USEmp_best
str(forc_USEmp_best)
hilo(forc_USEmp_best,level = c(80, 95))
autoplot(forc_USEmp_best, usemp)


#
#
#  Part 9
#

goog2015  <- filter(gafa_stock, Symbol == "GOOG", 
                    year(Date) == 2015)
goog2015 <- mutate(goog2015, day = row_number())
goog2015 <- update_tsibble(goog2015, index = day, 
                           regular = TRUE)  


fit_goog2015_best <- model(goog2015, 
                        ETS(Close ~ error("M") + trend("M"))
)
report(fit_goog2015_best)
accuracy(fit_goog2015_best)

# The best model is the ETS MM model with no seasonality because there is 
# no seasonality in the data according to the ets function.

# Smoothing parameters:
# alpha = 0.9998992 
# beta  = 0.0001000088


aug_goog2015_best <- augment(fit_goog2015_best)
autoplot(aug_goog2015_best, Close) +
  autolayer(aug_goog2015_best,.fitted, colour = "Red") +
  autolayer(aug_goog2015_best,.resid, colour = "Green")


forc_goog2015_best <- forecast(fit_goog2015_best, h = 30)
forc_goog2015_best
str(forc_goog2015_best)

autoplot(forc_goog2015_best, goog2015)


#
#
#  Part 10
#


autoplot(goog2015)
ACF(goog2015, lag_max = 12)
autoplot(ACF(goog2015, lag_max = 12))
PACF(goog2015, lag_max = 12)
autoplot(PACF(goog2015, lag_max = 12))

features(goog2015, Close, box_pierce, lag = 12, dof = 0)
features(goog2015, Close, ljung_box, lag = 12, dof = 0)


fit_g1 <- model(goog2015, ARIMA(Close))
report(fit_g1)
fit_g2 <- model(goog2015, ARIMA(Close, 
                                 stepwise = FALSE, approx = FALSE))
report(fit_g2)
fit_g0 <- model(goog2015, ARIMA(Close ~ pdq(0,1,0)))
report(fit_g0)

accuracy(fit_g1)

# The ARIMA model is better according to the AIC metric. 
 
# It was chosen because it has the lowest AICc metric
# The model chosen is as follows:-
# Series: Close 
# Model: ARIMA(0,1,1) 

# Coefficients:
#  ma1
#0.1190
#s.e.  0.0655

# The model RMSE is 11.1






