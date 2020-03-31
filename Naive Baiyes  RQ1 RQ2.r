install.packages("caret")
install.packages("ggplot2")
install.packages("lattice")
install.packages("e1071")
install.packages("rsample")
install.packages("naivebayes")

library(rsample)  # data splitting 
library(dplyr)  #data transformation
library(caret)
library(e1071)
library(naivebayes)

#Importing the complete dataset with over 9 million rows (9714896 to be precise)
Med16 <-read.csv(file.choose())
View(Med16)

#Creating a classifier for NB
Med16$Reimburse <- ifelse(Med16$percent_reimbursed > 90, 1,0)
Med16$Reimburse

#Creating a subset with only required fields
df16 <- subset(Med16, select = -c(2,3,4,5,8,9,11,12,13,16,10,17,18,19,20,21,22,23))
View(df16)



df16$National.Provider.Identifier <- as.factor(df16$National.Provider.Identifier)
df16$Gender.of.the.Provider <- as.factor(df16$Gender.of.the.Provider)
df16$Entity.Type.of.the.Provider <- as.factor(df16$Entity.Type.of.the.Provider)
df16$Provider.Type <- as.factor(df16$Provider.Type)
df16$Medicare.Participation.Indicator <- as.factor(df16$Medicare.Participation.Indicator)
df16$Average.Submitted.Charge.Amount <- as.factor(df16$Average.Submitted.Charge.Amount)
df16$Average.Medicare.Payment.Amount <- as.factor(df16$Average.Medicare.Payment.Amount)
df16$Average.Medicare.Standardized.Amount <- as.factor(df16$Average.Medicare.Standardized.Amount)
df16$percent_reimbursed <- as.factor(df16$percent_reimbursed)
df16$Year <- as.factor(df16$Year)
df16$Reimburse <- as.factor(df16$Reimburse)



#So we’ll be using some data to build a bayes classifier from this
# split the raw data into training and testing data
set.seed(123)
trainIndex=createDataPartition(df16$percent_reimbursed, p=0.8)$Resample1
train=df16[trainIndex, ] #240000 obvs
test= df16[-trainIndex, ] #60000 obvs


#check the balance
print(table(df16$Reimburse))
print(table(train$Reimburse))
print(table(test$Reimburse))

#Prediction for training and test data
(prop0 <- sum(train$Reimburse==0)/length(train$Reimburse))
(prop1 <- sum(train$Reimburse==1)/length(train$Reimburse))

#Build the model (Naive Bayes)
model <- naiveBayes(Reimburse~., data=train, usekernel = TRUE)
print(model)
actuals <- test$Reimburse

#PRedicting for NB
prediction <- predict(model, newdata = test)
print(prediction)

#creating confusion matrix
cm<-confusionMatrix(actuals,prediction)
cm

model_laplace<-naiveBayes(Reimburse ~ .,data=train,laplace = 1)
laplace_pred<-predict(model_laplace,newdata=test, type="class")
table(actuals,laplace_pred)



## Answering Research Question 2: 
#Importing the complete dataset with combined entries of 2014 and 2016
Comb16 <-read.csv(file.choose())
View(Comb16)

CB16 <- subset(Comb16, select = -c(2,4,6,7))

CB16$ID.2014 <- as.factor(CB16$ID.2014)
CB16$ID.2016 <- as.factor(CB16$ID.2016)
CB16$State.Code.of.the.Provider <- as.factor(CB16$State.Code.of.the.Provider)
CB16$Provider.Type <- as.factor(CB16$Provider.Type)
CB16$Medicare.Beneficiaries.2014 <- as.factor(CB16$Medicare.Beneficiaries.2014)
CB16$percent.2014 <- as.factor(CB16$percent.2014)
CB16$Percent.2016 <- as.factor(CB16$Percent.2016)
CB16$Medicare.Beneficiaries.2016 <- as.factor(CB16$Medicare.Beneficiaries.2016)


#So we’ll be using some data to build a bayes classifier from this
# split the raw data into training and testing data
set.seed(123)
trainIndex=createDataPartition(CB16$Medicare.Beneficiaries.2016, p=0.8)$Resample1
train=CB16[trainIndex, ] #240000 obvs
test= CB16[-trainIndex, ] #60000 obvs


#check the balance
print(table(CB16$Medicare.Beneficiaries.2016))
print(table(train$Medicare.Beneficiaries.2016))
print(table(test$Medicare.Beneficiaries.2016))


#Build the model (Naive Bayes)
model2 <- naiveBayes(Medicare.Beneficiaries.2016~., data=train, usekernel = TRUE)
print(model2)
actuals <- test$Medicare.Beneficiaries.2016

#PRedicting for NB
prediction <- predict(model2, newdata = test)
print(prediction)

#creating confusion matrix
cm<-confusionMatrix(actuals,prediction)
cm

model_laplace<-naiveBayes(Medicare.Beneficiaries.2016 ~ .,data=train,laplace = 1)
laplace_pred<-predict(model_laplace,newdata=test, type="class")
table(actuals,laplace_pred)




