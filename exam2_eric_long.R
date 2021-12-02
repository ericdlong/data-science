#BIA 6301
#Exam 2

#Eric Long

################################################################################

#remove scientific notation
options(scipen=999)

#add libraries
library(psych)
library(stargazer)
library(Metrics)
library(DiscriMiner)
library(leaps)
library(relaimpo)
library(ggplot2)
library(mltools)
library(rpart)
library(rpart.plot)
library(C50)
library(caret)
library(randomForest)

################################################################################

#Q1

tb_data<-read.csv("terabyte_purchase_data.csv", stringsAsFactors = TRUE)
attach(tb_data)
str(tb_data)
describe(tb_data)

#Q1.1
table(purchase)
#no = 257, yes = 143

#Q1.2
addmargins(table(gender,purchase))
#P(purchase/female) = 66/196 = 33.67%
#P(purchase/male) = 77/204 = 37.75%

#Q1.3
set.seed(777)
RANDOM_TB<-tb_data[order(runif(400)),]
RANDOM_TB
TRAIN_TB<-RANDOM_TB[1:300,]
TEST_TB<-RANDOM_TB[301:400,]
attach(TRAIN_TB)
FULL_TREE_TB<-rpart(purchase~.,data=TRAIN_TB,control=rpart.control(minsplit=2,minbucket=1,cp=0),method="class")
FULL_TREE_TB$cptable

#Q1.4
PRUNED_TREE_TB<-prune(FULL_TREE_TB,cp=0.0189)
PRUNED_TREE_TB
rpart.plot(PRUNED_TREE_TB,digits=2,fallen.leaves=T,type=2,extra=2)
#87 people over age 43, 75 purchased, 12 did not.
#213 people under age 43, 31 purchased, 182 did not.
#Age appears to be the most important predictor.

#Q1.5
PRED_TB<-predict(PRUNED_TREE_TB,TEST_TB,type="class")
PRED_TB
confusionMatrix(PRED_TB,TEST_TB$purchase,positive="yes")

#Q1.6
attach(TRAIN_TB)
LOG_TB<-glm(purchase~.,data=TRAIN_TB,family=binomial())
summary(LOG_TB)

#Q1.7
attach(TEST_TB)
PROB_TEST_TB<-predict(LOG_TB,TEST_TB,type="response")
describe(PROB_TEST_TB) # range between 0 and 1
PRED_TEST_TB<-ifelse(PROB_TEST_TB>0.50,"yes","no")
PRED_TEST_TB
confusionMatrix(as.factor(PRED_TEST_TB),TEST_TB$purchase)
#OR
table(PRED_TEST_TB,TEST_TB$purchase)
#accuracy = 83%

#Q1.8
#no R-code

################################################################################

#Q2

house_data<-read.csv("boston_housing_data.csv", stringsAsFactors = TRUE)
attach(house_data)
str(house_data)
describe(house_data)

#Q2.1
set.seed(555)
RANDOM_HOUSE<-house_data[order(runif(506)),]
RANDOM_HOUSE
TRAIN_HOUSE<-RANDOM_HOUSE[1:400,]
TEST_HOUSE<-RANDOM_HOUSE[401:506,]
attach(TRAIN_HOUSE)
FULL_TREE_HOUSE<-rpart(medv~.,data=TRAIN_HOUSE,control=rpart.control(minbucket=1,cp=0),method="anova")
options(max.print=99999)
FULL_TREE_HOUSE$cptable
#250 splits to reach cp=0

#Q2.2
PRUNED_TREE_HOUSE<-rpart(medv~.,data=TRAIN_HOUSE,control=rpart.control(minbucket=100),method="anova")
PRUNED_TREE_HOUSE$cptable
rpart.plot(PRUNED_TREE_HOUSE,digits=5,fallen.leaves=T,type=2,extra=1)
#lstat >= 7.65, 280/400 houses w/ avg. value = 18.847
#lstat < 7.65, 120/400 houses w/ avg. value = 31.9

#Q2.3
PRED_HOUSE<-predict(PRUNED_TREE_HOUSE,TEST_HOUSE)
PRED_HOUSE
rmse(TEST_HOUSE$medv,PRED_HOUSE) # = 7.0964
mae(TEST_HOUSE$medv,PRED_HOUSE) # = 5.3677

#Q2.4
MLR_HOUSE<-lm(medv~.,data=TRAIN_HOUSE)
summary(MLR_HOUSE)
MLR_PRED<-predict(MLR_HOUSE,TEST_HOUSE)
MLR_PRED
rmse(TEST_HOUSE$medv,MLR_PRED) # = 4.8826
mae(TEST_HOUSE$medv,MLR_PRED) # = 3.2054

#Q2.5
RF_HOUSE<-randomForest(medv~.,data=TRAIN_HOUSE,ntree=100,mtry=13,importance=TRUE)
RF_HOUSE
varImpPlot(RF_HOUSE,type=1) #rm, lstat, dis are 3 most important variables
RF_PRED<-predict(RF_HOUSE,TEST_HOUSE)
RF_PRED
rmse(TEST_HOUSE$medv,RF_PRED) # = 2.8358
mae(TEST_HOUSE$medv,RF_PRED) # = 2.05829

#Q2.6
#no R-code