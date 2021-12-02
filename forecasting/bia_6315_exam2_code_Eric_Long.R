#BIA 6315
#Week 8 - Exam 2
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
library(tseries)
library(zoo)


#####Q1#####

SALARY<-read.csv("salary_survey_data.csv", stringsAsFactors = TRUE)

#EDA
describe(SALARY)
str(SALARY)

#calculate mean of each gender group
aggregate(SALARY$salary, list(SALARY$gender), FUN=mean)

#get counts of grad degree males and females
addmargins(table(SALARY$gender, SALARY$graduate_degree))

#male, degree:
68/142

#female, degree:
28/58
 
#recode non numeric variables (gender, grad degree)

#code males=1, females=0
gender<-ifelse(SALARY$gender =="male", 1, 0)

#code degree=1, no degree=0
degree<-ifelse(SALARY$graduate_degree == "yes", 1, 0)

#combine recoded data into new data frame
NEW_SALARY<-data.frame(SALARY[c(-4,-5)], gender, degree)

#create correlation matrix
COR_MATRIX<-cor(NEW_SALARY)
COR_MATRIX

#linear regression
MODEL1<-lm(salary~., data=NEW_SALARY)
summary(MODEL1)

#predictions
MALE_OBS<-data.frame(firm_size=245, experience=15, gender=1, degree=1)
predict(MODEL1, MALE_OBS)

FEMALE_OBS<-data.frame(firm_size=245, experience=15, gender=0, degree=1)
predict(MODEL1, FEMALE_OBS)

#add interaction variables
INTERACTION<-data.frame(NEW_SALARY[], gen_deg=NEW_SALARY$gender*NEW_SALARY$degree, gen_exp=NEW_SALARY$gender*NEW_SALARY$experience)
MODEL2<-lm(salary~., data=INTERACTION)
summary(MODEL2)

#predictions
MALE_INT<-data.frame(firm_size=245, experience=15, gender=1, degree=1, gen_deg=1, gen_exp=15)
predict(MODEL2, MALE_INT)

FEMALE_INT<-data.frame(firm_size=245, experience=15, gender=0, degree=1, gen_deg=0, gen_exp=0)
predict(MODEL2, FEMALE_INT)

#regression subsets
REG_SUBS<-regsubsets(salary~., data=NEW_SALARY)
summary(REG_SUBS)

#beta regression

#first, standardize
Z_SALARY<-data.frame(scale(NEW_SALARY))
describe(Z_SALARY)

BETA_SALARY<-lm(salary~., data=Z_SALARY)
summary(BETA_SALARY)


#####Q2#####

CHARD<-read.csv("chardonnay_sales_data.csv")

#EDA
describe(CHARD)
str(CHARD)

#create time series
TS<-ts(CHARD$chardonnay, start = c(2006, 1), end = c(2020, 12), frequency = 12)
TS

#STL
STL<-stl(TS, "periodic")
STL
names(STL)
STL$time.series[,1]
plot.ts(STL$time.series[,1])

#autocorrelation
ACF<-Acf(TS, lag=12)
ACF

#take first difference
DIFF_TS<-diff(TS, differences=1)
DIFF_TS
describe(TS)
describe(DIFF_TS)
Acf(DIFF_TS, lag=12)

#split into training and test sets, test set = 24 months
TRAIN<-data.frame(CHARD[1:156,])
TEST<-data.frame(CHARD[157:180,])
describe(TRAIN)
describe(TEST)

#auto ARIMA on training set
TS_TRAIN<-ts(TRAIN$chardonnay, start = c(2006, 1), end = c(2018, 12), freq = 12)
describe(TS_TRAIN)
TS_TEST<-ts(TEST$chardonnay, start = c(2019, 1), end = c(2020, 12), freq = 12)
describe(TS_TEST)

AUTO_ARIMA<-auto.arima(TS_TRAIN, trace=TRUE)
AUTO_ARIMA

FORECASTS<-forecast(AUTO_ARIMA, h=24)
FORECASTS

#grab arima metrics
ARIMA_PRED<-FORECASTS$mean
ARIMA_PRED
TEST

mae(TEST$chardonnay,ARIMA_PRED)
rmse(TEST$chardonnay,ARIMA_PRED)
mape(TEST$chardonnay,ARIMA_PRED)

#polynomial model

#create a time and time-squared variable
time<-c(1:180)
time_sq<-(time * time)

#set ref month to jan
month_dummy<-relevel(as.factor(CHARD$month), ref='jan')

NEW_CHARD<-data.frame(time, time_sq, month_dummy, chardonnay=CHARD$chardonnay)
NEW_CHARD

#create train/test sets
NEW_TRAIN<-data.frame(NEW_CHARD[1:156,])
NEW_TEST<-data.frame(NEW_CHARD[157:180,])

POLY_MODEL<-lm(chardonnay~time+time_sq+month_dummy, data=NEW_TRAIN)
summary(POLY_MODEL)

#poly model metrics
POLY_PRED<-predict(POLY_MODEL, NEW_TEST)
POLY_PRED
NEW_TEST

mae(NEW_TEST$chardonnay,POLY_PRED)
rmse(NEW_TEST$chardonnay,POLY_PRED)
mape(NEW_TEST$chardonnay,POLY_PRED)

#get residuals of poly model
RESIDUALS<-resid(POLY_MODEL)
RESIDUALS
 
#look at ACF of residuals
ACF_RESID<-Acf(RESIDUALS, lag=12)

#run ARIMA(1,0,0) on residuals
ECM<-arima(RESIDUALS, c(1,0,0))
ECM

#get error adjustment
ECM_FORECASTS<-forecast(ECM, h=24)
ECM_FORECASTS #first = 27.37


#####Q3#####

#Hand calculations, no code.
