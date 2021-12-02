#BIA 6301
#Exam 1

#Eric Long

################################################################################

#Set options and libraries

options(scipen=999)
library(psych)
library(stargazer)
library(Metrics)
library(DiscriMiner)
library(leaps)
library(relaimpo)

################################################################################

#Q1

college_gpa_data<-read.csv("college_gpa_data.csv")
attach(college_gpa_data)
describe(college_gpa_data)
str(college_gpa_data)

#linear regression model
LM_Q1<-lm(GPA~.,data=college_gpa_data)
summary(LM_Q1)

#subsets
SUB_MODEL_Q1<-regsubsets(GPA~.,data=college_gpa_data,nvmax=10)
summary(SUB_MODEL_Q1)

TOTAL_POSS<-(choose(10,1)+choose(10,2)+choose(10,3)+choose(10,4)+choose(10,5)+choose(10,6)+choose(10,7)+
    choose(10,8)+choose(10,9)+choose(10,10))
TOTAL_POSS

SUB_SUMMARY<-summary(SUB_MODEL_Q1)
SUB_SUMMARY$adjr2

BEST5VAR<-lm(GPA~SAT+total_hours+hs_percentile+female+black,data=college_gpa_data)
summary(BEST5VAR)

#RMSE
FITTED_Q1<-fitted(BEST5VAR)
rmse(GPA,FITTED_Q1)

################################################################################

#Q2

lv_mortgage_data<-read.csv("las_vegas_mortgage_data.csv")
attach(lv_mortgage_data)
describe(lv_mortgage_data)
str(lv_mortgage_data)

#proportion tables
table(delinquent)
199/1000 #=0.199 or ~20%

table(mortgage_insurance)
720/1000 #=0.72 or 72%

table(ARM)
716/1000 #=0.716 or ~72%

addmargins(table(delinquent,mortgage_insurance))
47/1000 #=0.047 or 4.7%

addmargins(table(delinquent,ARM))
159/1000 #=0.159  or ~16%

addmargins(table(delinquent,mortgage_insurance,ARM))
#33 overlapping cases where borrower has both insurance and ARM

#LPM
LPM<-lm(delinquent~.,data=lv_mortgage_data)
summary(LPM)

FITTED_Q2<-fitted(LPM)
describe(FITTED_Q2)
FITTED_Q2[1000]

#Logistic Regression
LOGIT_MODEL<-glm(as.factor(delinquent)~.,data=lv_mortgage_data,family=binomial())
summary(LOGIT_MODEL)
#using only ARM coef:
#formula: ln(delinquent) = 1.665 + 1.406*ARM = 1.665 + 1.406 = 3.071
exp(3.071) #= 21.56
21.56/22.56 #= 0.956 or 96%


PROB<-data.frame(fitted(LOGIT_MODEL))
describe(PROB)
PREDICTED<-ifelse(PROB>0.20,1,0)
PREDICTED
CONFUSION_MATRIX<-table(delinquent,PREDICTED)
CONFUSION_MATRIX

ACCURACY<-(675+154)/(675+45+126+154)
ACCURACY # = 0.829 or 83%

#LDA
LDA_MODEL<-linDA(lv_mortgage_data[,2:9],as.factor(lv_mortgage_data[,1]))
LDA_MODEL$confusion
1-LDA_MODEL$error_rate #=0.859
#confirm
(711+148)/(711+148+90+51) #=0.859
