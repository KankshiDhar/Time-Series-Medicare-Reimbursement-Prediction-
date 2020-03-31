#Question 4: COMPARITIVE ANALYSIS

#RESEARCH QUESTION 1
#Using the K-folds method of cross validation

install.packages("caret") 
install.packages("e1071") 

library(caret) 
library(e1071) 

set.seed(100)

#RESEARCH QUESTION 1 : Using the CV method K-fold
#Importing the csv file
fourth2 <- read.csv(file.choose(), header = T)

#Because R is throwing memory related error, we are working with a small sample of 1000 rows
random<-fourth2[sample(nrow(fourth2), 1000),]
View(random)

#Creating a binary variable for percent_reimbursed
random$percent_reimbursed <- ifelse(random$percent_reimbursed >90,1,0)

#Converting percent_reimbursed into a factor for applying the classifiers
random$percent_reimbursed <- as.factor(random$percent_reimbursed)

str(random)
#Defining train_control 
train_control <- trainControl(method = "cv", number = 100)

#Running the training model which is a part of the Caret package
model1 <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries), data = random, method = "nb", trControl = train_control)
warnings()
summary(model1)

#Predictions and Confusion Matrix
predictions <- predict(model1, random[,c("Number.of.Services", "Number.of.Medicare.Beneficiaries")])
confusionMatrix(predictions, random$percent_reimbursed)

#Comparing classifier results
install.packages("mlbench")
library(mlbench)
library(caret)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(7)
modelNB <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries) + , data = random, method = "nb", trControl = control)
summary(modelNB)

modelSVM <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries), data = random, method = "svmRadial", trControl = control)
summary(modelSVM)

results <- resamples(list(NB = modelNB, SVM = modelSVM))
summary(results)

#Using the LOOCV Method of Cross Validation
#Importing the csv file
fourth2 <- read.csv(file.choose(), header = T)

#Because R is throwing memory related error, we are working with a small sample of 1000 rows
random<-fourth2[sample(nrow(fourth2), 500),]
View(random)

#Creating a binary variable for percent_reimbursed
random$percent_reimbursed <- ifelse(random$percent_reimbursed >90,1,0)

#Converting percent_reimbursed into a factor for applying the classifiers
random$percent_reimbursed <- as.factor(random$percent_reimbursed)

str(random)
#Defining train_control 
train_control <- trainControl(method = "LOOCV")

#Running the training model which is a part of the Caret package
model1 <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries), data = random, method = "nb", trControl = train_control)
warnings()
summary(model1)

#Predictions and Confusion Matrix
predictions <- predict(model1, random[,c("Number.of.Services", "Number.of.Medicare.Beneficiaries")])
confusionMatrix(predictions, random$percent_reimbursed)

#Comparing classifier results
install.packages("mlbench")
library(mlbench)
library(caret)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(7)
modelNB <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries) + , data = random, method = "nb", trControl = control)
summary(modelNB)

modelSVM <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries), data = random, method = "svmRadial", trControl = control)
summary(modelSVM)

results <- resamples(list(NB = modelNB, SVM = modelSVM))
summary(results)

#Using the LGOCV method of Cross Validation
#Importing the csv file
fourth2 <- read.csv(file.choose(), header = T)

#Because R is throwing memory related error, we are working with a small sample of 1000 rows
random<-fourth2[sample(nrow(fourth2), 500),]
View(random)

#Creating a binary variable for percent_reimbursed
random$percent_reimbursed <- ifelse(random$percent_reimbursed >90,1,0)

#Converting percent_reimbursed into a factor for applying the classifiers
random$percent_reimbursed <- as.factor(random$percent_reimbursed)

str(random)
#Defining train_control 
train_control <- trainControl(method = "LGOCV")

#Running the training model which is a part of the Caret package
model1 <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries), data = random, method = "nb", trControl = train_control)
warnings()
summary(model1)

#Predictions and Confusion Matrix
predictions <- predict(model1, random[,c("Number.of.Services", "Number.of.Medicare.Beneficiaries")])
confusionMatrix(predictions, random$percent_reimbursed)

#Comparing classifier results
install.packages("mlbench")
library(mlbench)
library(caret)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(7)
modelNB <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries) , data = random, method = "nb", trControl = control)
summary(modelNB)

modelSVM <- train(percent_reimbursed ~ as.factor(Number.of.Services) + as.factor(Number.of.Medicare.Beneficiaries), data = random, method = "svmRadial", trControl = control)
summary(modelSVM)

results <- resamples(list(NB = modelNB, SVM = modelSVM))
summary(results)


