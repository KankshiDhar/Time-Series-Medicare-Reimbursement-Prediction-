#INTRODUCTION TO DATA SCIENCE
#Milestone 3
#Team 6

install.packages("caret") 
install.packages("e1071") 

library(caret) 
library(e1071) 

set.seed(100)

#RESEARCH QUESTION 2 : Using the CV method K-fold
#Importing the csv file
research1 <- read.csv(file.choose(), header = T)

#Because R is throwing memory related error, we are working with a small sample of 1000 rows
random<-research1[sample(nrow(research1), 500),]
View(random)

#Creating a binary variable for percent_reimbursed
random$Medicare.Beneficiaries.2016 <- ifelse(random$Medicare.Beneficiaries.2016 > random$Medicare.Beneficiaries.2014,1,0)

#Converting percent_reimbursed into a factor for applying the classifiers
random$Medicare.Beneficiaries.2016 <- as.factor(random$Medicare.Beneficiaries.2016)

str(random)
#Defining train_control 
train_control <- trainControl(method = "cv", number = 100)

#Running the training model which is a part of the Caret package
model1 <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014) , data = random, method = "nb", trControl = train_control)
warnings()
summary(model1)

#Predictions and Confusion Matrix
predictions <- predict(model1, random[,c("percent.2014")])
confusionMatrix(predictions, random$Medicare.Beneficiaries.2016)

#Comparing classifier results
install.packages("mlbench")
library(mlbench)
library(caret)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(7)
modelNB <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014) , data = random, method = "nb", trControl = control)
summary(modelNB)

modelSVM <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014), data = random, method = "svmRadial", trControl = control)
summary(modelSVM)

results <- resamples(list(NB = modelNB, SVM = modelSVM))
summary(results)

#Using the Cross Validation method of LOOCV

#Importing the csv file
research1 <- read.csv(file.choose(), header = T)

#Because R is throwing memory related error, we are working with a small sample of 1000 rows
random<-research1[sample(nrow(research1), 500),]
View(random)

#Creating a binary variable for percent_reimbursed
random$Medicare.Beneficiaries.2016 <- ifelse(random$Medicare.Beneficiaries.2016 > random$Medicare.Beneficiaries.2014,1,0)

#Converting percent_reimbursed into a factor for applying the classifiers
random$Medicare.Beneficiaries.2016 <- as.factor(random$Medicare.Beneficiaries.2016)

str(random)
#Defining train_control 
train_control <- trainControl(method = "LOOCV")

#Running the training model which is a part of the Caret package
model1 <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014) , data = random, method = "nb", trControl = train_control)
warnings()
summary(model1)

#Predictions and Confusion Matrix
predictions <- predict(model1, random[,c("percent.2014")])
confusionMatrix(predictions, random$Medicare.Beneficiaries.2016)

#Comparing classifier results
install.packages("mlbench")
library(mlbench)
library(caret)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(7)
modelNB <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014) , data = random, method = "nb", trControl = control)
summary(modelNB)

modelSVM <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014), data = random, method = "svmRadial", trControl = control)
summary(modelSVM)

results <- resamples(list(NB = modelNB, SVM = modelSVM))
summary(results)

#Using the Cross Validation method of LGOCV
install.packages("caret") 
install.packages("e1071") 

library(caret) 
library(e1071) 

set.seed(100)

#RESEARCH QUESTION 1 : Using the CV method K-fold
#Importing the csv file
research1 <- read.csv(file.choose(), header = T)

#Because R is throwing memory related error, we are working with a small sample of 1000 rows
random<-research1[sample(nrow(research1), 500),]
View(random)

#Creating a binary variable for percent_reimbursed
random$Medicare.Beneficiaries.2016 <- ifelse(random$Medicare.Beneficiaries.2016 > random$Medicare.Beneficiaries.2014,1,0)

#Converting percent_reimbursed into a factor for applying the classifiers
random$Medicare.Beneficiaries.2016 <- as.factor(random$Medicare.Beneficiaries.2016)

str(random)
#Defining train_control 
train_control <- trainControl(method = "LGOCV")

#Running the training model which is a part of the Caret package
model1 <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014) , data = random, method = "nb", trControl = train_control)
warnings()
summary(model1)

#Predictions and Confusion Matrix
predictions <- predict(model1, random[,c("percent.2014")])
confusionMatrix(predictions, random$Medicare.Beneficiaries.2016)

#Comparing classifier results
#install.packages("mlbench")
library(mlbench)
library(caret)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(7)
modelNB <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014) , data = random, method = "nb", trControl = control)
summary(modelNB)

modelSVM <- train(Medicare.Beneficiaries.2016 ~ as.factor(percent.2014), data = random, method = "svmRadial", trControl = control)
summary(modelSVM)

results <- resamples(list(NB = modelNB, SVM = modelSVM))
summary(results)






































