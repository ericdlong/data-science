#BIA 6315
#Week 4 - Exam 1
#Eric Long

#####set options and import libraries#####
options(scipen = 999)
options(digits = 10)

library(Metrics)
library(stargazer)
library(DAAG)
library(caret)
library(psych)
library(leaps)
library(relaimpo)
library(forecast)

#####Q1#####
boston<-read.csv('boston_housing_data.csv', stringsAsFactors = TRUE)

#EDA
describe(boston)
str(boston)
dim(boston)

#set seed and randomize data
set.seed(555)
boston_random<-boston[order(runif(506)),]

#create training set
boston_train<-boston_random[1:406,]

#create test set
boston_test<-boston_random[407:506,]

#run multiple linear regression, outcome variable = medv
boston_mlr<-lm(medv~., data=boston_train)
summary(boston_mlr)

#report mae, rmse, mape for the training set
predicted_boston<-fitted(boston_mlr)
predicted_boston

mae(boston_train$medv, predicted_boston)
rmse(boston_train$medv, predicted_boston)
mape(boston_train$medv, predicted_boston)

#apply model to test set
test_predictions_boston<-predict(boston_mlr, boston_test)
test_predictions_boston

#report mae, rmse, mape for the test set
mae(boston_test$medv, test_predictions_boston)
rmse(boston_test$medv, test_predictions_boston)
mape(boston_test$medv, test_predictions_boston)

#find the best 3-variable model
boston_subsets<-regsubsets(medv~., data=boston_train)
summary(boston_subsets)

#find the coefficients and adjusted R^2 for the 3-variable model
boston_3var<-lm(medv~rm+ptratio+lstat, data=boston_train)
summary(boston_3var)

#run 10-fold cross validation on the 3-variable model using the entire data set
set.seed(555)
boston_cv_model<-cv.lm(data=boston, form.lm=boston_3var, m=10, plotit=TRUE)

#report mae, rmse, mape for the cv model
mae(boston_cv_model$medv, boston_cv_model$cvpred)
rmse(boston_cv_model$medv, boston_cv_model$cvpred)
mape(boston_cv_model$medv, boston_cv_model$cvpred)

#run a beta regression on the entire data set
z_boston<-data.frame(scale(boston))
describe(z_boston) #verify mean=0 and sd=1

beta_boston<-lm(medv~., data=z_boston)
summary(beta_boston)

#apply relaimpo using the entire data set
boston_full_model<-lm(medv~., data=boston)
summary(boston_full_model)

calc.relimp(boston_full_model, type="lmg", rela=TRUE)

#####Q2#####
ragsdale<-read.csv("ragsdale_sales_data.csv", stringsAsFactors = T)

#EDA
describe(ragsdale)
str(ragsdale)
dim(ragsdale)

#create time series model
ragsdale_ts<-ts(ragsdale$sales, start = c(2015,1), end = c(2020,4), frequency = 4)
ragsdale_ts

#decompose the dataset using STL method
ragsdale_stl<-stl(ragsdale_ts, "periodic")
ragsdale_stl

#find mean value of trend
names(ragsdale_stl)
mean(ragsdale_stl$time.series[,2])

#compare to mean value of sales in non-decomposed data
mean(ragsdale$sales)

#which quarter for highest positive seasonality?
max(ragsdale_stl$time.series[,1])

#mean value of remainder
mean(ragsdale_stl$time.series[,3])

#create a time-counter variable for our data
time<-c(1:24)

#create a skewed variable since we will be doing a polynomial model
time_sq<-(time * time)

#set Q1 as reference quarter
q_dummy<-relevel(as.factor(ragsdale$quarter), ref='Q1')

#create a new data set using the above defined variables
new_rags<-data.frame(time, time_sq, q_dummy, ragsdale$sales)
new_rags

#create training set
rags_train<-data.frame(new_rags[1:20,])

#create test set
rags_test<-data.frame(new_rags[21:24,])

#run linear model with seasonality
rags_lm<-lm(ragsdale.sales~time+q_dummy, data = rags_train)
summary(rags_lm)

#predict against test set
rags_lm_pred<-predict(rags_lm, rags_test)
rags_lm_pred

#predict 2021:Q1 value
obs_2021_q1<-data.frame(time=25, time_sq=25*25, q_dummy=as.factor('Q1'))
predict(rags_lm, obs_2021_q1)

#predict 2021:Q4 value
obs_2021_q4<-data.frame(time=28, time_sq=28*28, q_dummy=as.factor('Q4'))
predict(rags_lm, obs_2021_q4)

#calculate rmse of test set
rmse(rags_test$ragsdale.sales, rags_lm_pred)

#run polynomial model with seasonality
rags_poly<-lm(ragsdale.sales~time+time_sq+q_dummy, data = rags_train)
summary(rags_poly)

#predict against the test set
rags_poly_pred<-predict(rags_poly, rags_test)
rags_poly_pred

#predict 2021:Q1 value
predict(rags_poly, obs_2021_q1)

#calculate rmse of test set
rmse(rags_test$ragsdale.sales, rags_poly_pred)

#####Q3#####
hedge<-read.csv("hedge_fund_revenue_data.csv", stringsAsFactors = T)

#EDA
describe(hedge)
str(hedge)
dim(hedge)

#create time series object
hedge_ts<-ts(hedge$revenue, start = c(2018,1), end = c(2020,12), frequency = 12)
hedge_ts

#plot time series object
plot.ts(hedge_ts, col='blue', main='Monthly Hedge Fund Revenue', xlab='Month', ylab='Revenue')

#decompose data
hedge_stl<-stl(hedge_ts, "periodic")
hedge_stl

#plot trend
plot.ts(hedge_stl$time.series[,2], col='blue', main='Hedge Trend Component', xlab='Month', ylab='Trend')

#plot seasonality
plot.ts(hedge_stl$time.series[,1], col='blue', main='Hedge Seasonal Component', xlab='Month', ylab='Seasonality')

#plot random/remainder
plot.ts(hedge_stl$time.series[,3], col='blue', main='Hedge Random Component', xlab='Month', ylab='Remainder')

#create a time-counter variable for our data
htime<-c(1:36)

#create a skewed variable since we will be doing a polynomial model
htime_sq<-(htime * htime)

#set Q1 as reference quarter
m_dummy<-relevel(as.factor(c('jan','feb','mar','apr','may','jun',
                             'jul','aug','sep','oct','nov','dec')), ref='jan')

#create a new data set using the above defined variables
revenue<-hedge$revenue
new_hedge<-data.frame(htime, htime_sq, m_dummy, revenue)
new_hedge

#create training set
hedge_train<-data.frame(new_hedge[1:24,])

#create test set
hedge_test<-data.frame(new_hedge[25:36,])

#model 1 - linear, no seasonality
model<-lm(revenue~htime, data=hedge_train)
summary(model)

pred<-predict(model, hedge_test)
pred

actual<-hedge_test$revenue

rmse(actual, pred)
mae(actual, pred)
mape(actual, pred)

#model 2 - linear, seasonality
model<-lm(revenue~htime+m_dummy, data=hedge_train)
summary(model)

pred<-predict(model, hedge_test)
pred

rmse(actual, pred)
mae(actual, pred)
mape(actual, pred)

#model 3 - exponential, no seasonality
model<-lm(log(revenue)~htime, data=hedge_train)
summary(model)

pred<-predict(model, hedge_test)
pred

rmse(actual, pred)
mae(actual, pred)
mape(actual, pred)

#model 4 - exponential, seasonality
model<-lm(log(revenue)~htime+m_dummy, data=hedge_train)
summary(model)

pred<-predict(model, hedge_test)
pred

rmse(actual, pred)
mae(actual, pred)
mape(actual, pred)

#model 5 - polynomial, no seasonality
model<-lm(revenue~htime+htime_sq, data=hedge_train)
summary(model)

pred<-predict(model, hedge_test)
pred

rmse(actual, pred)
mae(actual, pred)
mape(actual, pred)

#model 6 - polynomial, seasonality
model<-lm(revenue~htime+htime_sq+m_dummy, data=hedge_train)
summary(model)

pred<-predict(model, hedge_test)
pred

rmse(actual, pred)
mae(actual, pred)
mape(actual, pred)

#predict Dec 2021 revenue using best model (5)
model5<-lm(revenue~htime+htime_sq, data=hedge_train)

obs_dec2021<-data.frame(htime=48, htime_sq=48*48, m_dummy=as.factor('dec'))
predict(model5, obs_dec2021)
