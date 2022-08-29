#
#  Answers to Homework Assignment 6
#  BA 288 Winter 2022
#  Murphy 2/27/22 Draft
#
#
#  Read in dataset 
#

library(fpp3)
install.packages("forecast")
library(forecast)

#
#
#  Part 1.  
#
# a.

wn_time <- ts(data.frame(rnorm(1000)))
wnts <- as_tsibble(wn_time)
names(wnts)
autoplot(wnts, value)
ACF(wnts, lag_max = 20)
PACF(wnts, lag_max = 20)
autoplot(ACF(wnts, lag_max = 20))
# The pattern is in no way related for ACF.
autoplot(PACF(wnts, lag_max = 20))
# The pattern is in no way related for PACF.
#
#
#  Part 1.  
#
# b.
set.seed(333222)
wn_time <- ts(data.frame(rnorm(200)))
ysim1 <- ts(data.frame(matrix(rep(0),200,1)))
ysim1[1,1] <- wn_time[1]
for (i in 2:200) {
  ysim1[i,1] <- 0.6*ysim1[i-1,1] + wn_time[i] 
}
ysim1 <- as_tsibble(ysim1)
str(ysim1)
autoplot(ysim1, value)
ACF(ysim1, value)
autoplot(ACF(ysim1, value))
# The pattern is substantially related.
PACF(ysim1, value)
autoplot(PACF(ysim1, value))
# The pattern is related only for the first value.

#
#  Part 1.  
#
# c.

ysim3 <- ts(data.frame(matrix(rep(0),200,1)))
ysim3[1,1] <- wn_time[1]
ysim3[2,1] <- wn_time[2]
for (i in 3:200) {
  ysim3[i,1] <- 0.6*ysim3[i-1,1] + 
    0.3*ysim3[i-2,1] + wn_time[i] 
}
ysim3 <- as_tsibble(ysim3)
autoplot(ysim3, value)

ACF(ysim3, value)
autoplot(ACF(ysim3, value))
# The pattern is related substantially
PACF(ysim3, value)
autoplot(PACF(ysim3, value))
# The pattern is related only for the first value.


#
#  Part 1.  
#
# d.

ysim3 <- ts(data.frame(matrix(rep(0),200,1)))
ysim3[1,1] <- wn_time[1]
ysim3[2,1] <- wn_time[2]
for (i in 3:200) {
  ysim3[i,1] <- 0.8*ysim3[i-1,1] + 
    - 0.3*ysim3[i-2,1] + wn_time[i] 
}
ysim3 <- as_tsibble(ysim3)
autoplot(ysim3, value)

ACF(ysim3, value)
autoplot(ACF(ysim3, value))
# Only the first value is correlated. The rest are not.
PACF(ysim3, value)
autoplot(PACF(ysim3, value))
# Only the first value is correlated. The rest are not.


#
#  Part 1.  
#
# e.

ysim5 <- ts(data.frame(matrix(rep(0),200,1)))
ysim5[1,1] <- wn_time[1]
for (i in 2:200) {
  ysim5[i,1] <- wn_time[i] + 0.6*wn_time[i-1] 
}
ysim5 <- as_tsibble(ysim5)
autoplot(ysim5, value)

ACF(ysim5)
PACF(ysim5)

ACF(ysim5, value)
autoplot(ACF(ysim5, value))
# Only the first value is correlated. The rest are not.
PACF(ysim5, value)
autoplot(PACF(ysim5, value))
# Only the first few values are correlated. The rest are not.
#
#  Part 1.  
#
# f.

ysim6 <- ts(data.frame(matrix(rep(0),200,1)))
ysim6[1,1] <- wn_time[1]
#ysim6[2,1] <- wn_time[2]
for (i in 3:200) {
  ysim6[i,1] <- 0.5*ysim6[i-1,1] + 
    + wn_time[i] + 0.4*wn_time[i-1]
}
ysim6 <- as_tsibble(ysim6)
autoplot(ysim6, value)

ACF(ysim6, value)
autoplot(ACF(ysim6, value))
# There is some correlation for the first 3 values.
PACF(ysim6, value)
autoplot(PACF(ysim6, value))
# There is some correlation for the first value.

#
#  Part 1.  
#
# g

wn_time <- ts(data.frame(rnorm(200)))
wnts1 <- as_tsibble(wn_time)

#fit_4l <- model(wnts1,g = arima(wnts1, order=c(1,1,1),fixed=c(0.5, 0.4)))
g = arima(wn_time, order=c(1,1,1),fixed=c(0.5, 0.4))

fitted(g)
plot(wn_time)
plot(fitted(g))

class(fitted(g))

ysim7 <- as_tsibble(fitted(g))
ACF(ysim7)
autoplot(ACF(ysim7,value))
# There is negative correlation for the first value.
PACF(ysim7)
autoplot(PACF(ysim7,value))
# There is negative correlation for the first 2 values.
#
#  Part 1.  
#
# h


wn_time <- ts(data.frame(rnorm(200)))
wnts2 <- as_tsibble(wn_time)
h = arima(wn_time, order=c(1,1,1),seasonal = list(order = c(0,1,0)),fixed=c(0.5, 0.4))

fitted(h)
plot(wn_time)
plot(fitted(h))

class(fitted(h))

ysim8 <- as_tsibble(fitted(h))
ACF(ysim8,value)
autoplot(ACF(ysim8,value))
# There is substantial negative correlation for the first value.
PACF(ysim8,value)
autoplot(PACF(ysim8,value))

# There is substantial negative correlation for the first 2 values.



#
#
#  Part 2.  
#
# a.


dat2 <- read.csv("hw6_USGDP.csv")
str(dat2)
GDPt <- ts(dat2[,2], frequency = 4, start = 1947)
GDPts <- as_tsibble(GDPt)
names(GDPts)[2] <- "GDP"
str(GDPts)
autoplot(GDPts, GDP) 


features(GDPts, GDP, features = guerrero)
lambda <- pull(features(GDPts, GDP, features = guerrero), lambda_guerrero)
GDPts <- mutate(GDPts, GDPT = box_cox(GDP, lambda)) 

autoplot(GDPts, GDPT)

#
#
#  Part 2.  
#
# b.

fit_GDP <- model(GDPts, ARIMA(box_cox(GDP, lambda)))

#
#
#  Part 2.  
#
# c.


fit_2l <- model(GDPts, AS1 = ARIMA(box_cox(GDP, lambda) ~ pdq(3,0,0)), AS2 = ARIMA(box_cox(GDP, lambda) ~ pdq(1,0,0)),AS3 = ARIMA(box_cox(GDP, lambda) ~ pdq(1,0,1)))
fit_22 <- model(GDPts, AS1 = ARIMA(box_cox(GDP, lambda)))
report(fit_22)
glance(fit_22)

report(fit_2l)
glance(fit_2l)

augment(fit_2l)
aug_1 <- augment(fit_2l)
aug_1$.resid


#
#
#  Part 2.  
#
# d.

gg_tsresiduals(select(fit_2l,AS1))
gg_tsresiduals(select(fit_2l,AS2))
gg_tsresiduals(select(fit_2l,AS3))

#The AIC is the best for model 2.



#
#
#  Part 2.  
#
# e.

forc_4 <- forecast(fit_2l, h = 60)
forc_4_a <- filter(forc_4, .model == "AS2")
autoplot(forc_4_a, GDPts)

#
#
#  Part 2.  
#
# f.

fit_JJ_SES <- model(GDPts, 
                    ETS(box_cox(GDP, lambda))
)
accuracy(fit_JJ_SES)

report(fit_JJ_SES)

# The ETS model performs better than ARIMA in this case.


#
#
#  Part 3.  
#
# a.


dat2 <- read.csv("hw6_one_family_homes.csv")
str(dat2)
Sales <- ts(dat2[,2], frequency = 4, start = 1963)
Salests <- as_tsibble(Sales)
names(Salests)[2] <- "Sales"
str(Salests)
autoplot(Salests, Sales) 


features(Salests, Sales, features = guerrero)
lambda <- pull(features(Salests, Sales, features = guerrero), lambda_guerrero)
Salests <- mutate(Salests, SalesT = box_cox(Sales, lambda)) 

autoplot(Salests, SalesT)

#
#
#  Part 2.  
#
# b.

fit_Sales <- model(Salests, ARIMA(box_cox(Sales, lambda)))

#
#
#  Part 2.  
#
# c.


fit_2l <- model(Salests, AS1 = ARIMA(box_cox(Sales, lambda) ~ pdq(3,0,0)), AS2 = ARIMA(box_cox(Sales, lambda) ~ pdq(1,0,0)),AS3 = ARIMA(box_cox(Sales, lambda) ~ pdq(1,0,1)))
fit_22 <- model(Salests, AS1 = ARIMA(box_cox(Sales, lambda)))
report(fit_22)
glance(fit_22)

report(fit_2l)
glance(fit_2l)

augment(fit_2l)
aug_1 <- augment(fit_2l)
aug_1$.resid


#
#
#  Part 2.  
#
# d.

gg_tsresiduals(select(fit_2l,AS1))
gg_tsresiduals(select(fit_2l,AS2))
gg_tsresiduals(select(fit_2l,AS3))

#The AIC is the best for model 1.



#
#
#  Part 2.  
#
# e.

forc_4 <- forecast(fit_2l, h = 60)
forc_4_a <- filter(forc_4, .model == "AS2")
autoplot(forc_4_a, Salests)

#
#
#  Part 2.  
#
# f.

fit_JJ_SES <- model(Salests, 
                    ETS(box_cox(Sales, lambda))
)
accuracy(fit_JJ_SES)

report(fit_JJ_SES)

# The ETS model performs worse than ARIMA in this case.


















