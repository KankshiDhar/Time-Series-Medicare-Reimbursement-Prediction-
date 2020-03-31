install.packages("kernlab")
library(rsample)  # data splitting 
library(dplyr)  #data transformation
library(e1071)
library(caret)  #Using for Data Partition Essentially
library(kernlab)

RQ1 <- read.csv(file.choose())
str(RQ1)

#Creating a classifier variable for SVM
RQ1$Reimburse <- ifelse(RQ1$percent_reimbursed > 90, 1,0)
RQ1$Reimburse
df1 <- data.frame(RQ1$percent_reimbursed, RQ1$Number.of.Services, 
      RQ1$Number.of.Medicare.Beneficiaries, RQ1$Average.Medicare.Payment.Amount, RQ1$Reimburse)

#Subsetting data into smaller variables since svm classifier becomes
#a heavy vectror unable to load on a smaller RAM
View(df1)

trainIndex=createDataPartition(df1$RQ1.percent_reimbursed, p=0.8)$Resample1
RQ1_train = df1[trainIndex, ] #12132 obvs
RQ1_test = df1[-trainIndex, ] #48539 obvs


dim(RQ1_test)
dim(RQ1_train)

RQ1 <- is.na(df1)
anyNA(RQ1) #False
summary(RQ1)

RQ1_train[["percent_reimbursed"]] = factor(RQ1_train[["percent_reimbursed"]])
RQ1_train$percent_reimbursed  #779 unique levels

####Using Kernel Vanilladot
classifier <- ksvm(df1$RQ1.Reimburse ~ df1$RQ1.Number.of.Services + 
                     df1$RQ1.Number.of.Medicare.Beneficiaries + 
                     df1$RQ1.Average.Medicare.Payment.Amount, 
                   kernel = "vanilladot")
classifier

SV1 <- RQ1_train[alphaindex(classifier)[[2]],]
plot(SV1,pch= 0,main="Support vectors - linear model")

train.pred = predict(classifier, RQ1_test)
head(train.pred)
Matrix <- unlist(train.pred, RQ1_test$RQ1.Reimburse)
table(Matrix)


####Using Kernel rbfdot
Model2 <- ksvm(df1$RQ1.Reimburse ~ df1$RQ1.Number.of.Services + 
                     df1$RQ1.Number.of.Medicare.Beneficiaries + 
                     df1$RQ1.Average.Medicare.Payment.Amount, 
                   kernel = "rbfdot")
Model2
summary(Model2)

train = predict(Model2, RQ1_test)
head(train)

Matrix2 <- unlist(train, RQ1_test$RQ1.percent_reimbursed)
table(Matrix2)


#Code for Research Question #02 
#Finding if Reimbursement rates affect 2016
#Importing the complete dataset with combined entries of 2014 and 2016

Comb16 <-read.csv(file.choose())
View(Comb16)

df2 <- data.frame(Comb16$percent.2014, Comb16$Percent.2016, Comb16$Medicare.Beneficiaries.2014,
                  Comb16$Medicare.Beneficiaries.2016)
View(df2)

trainIndex=createDataPartition(df2$Comb16.Medicare.Beneficiaries.2014, p=0.8)$Resample1
RQ2_train = df2[trainIndex, ] #942 obvs
RQ2_test = df2[-trainIndex, ] #234 obvs

dim(RQ2_test)
dim(RQ2_train)
anyNA(df2) #False
summary(df2)
View(df2)

####Using Kernel Vanilladot
RQ2_classifier <- ksvm(df2$Comb16.Medicare.Beneficiaries.2014 ~ df2$Comb16.percent.2014
                       + df2$Comb16.Percent.2016 + df2$Comb16.Medicare.Beneficiaries.2016,
                   kernel = "vanilladot")
RQ2_classifier
summary(RQ2_classifier)
plot(RQ2_classifier, df2$Comb16.Medicare.Beneficiaries.2014)

train.pred2 = predict(RQ2_classifier, RQ2_test)
head(train.pred2)

misclass <- table(predict = train.pred2, truth = df2$Comb16.Medicare.Beneficiaries.2014)


####Using Kernel rbfdot
ModelRQ2 <- ksvm(df2$Comb16.Medicare.Beneficiaries.2014 ~ df2$Comb16.percent.2014
                 + df2$Comb16.Percent.2016 + df2$Comb16.Medicare.Beneficiaries.2016,
               kernel = "rbfdot")
ModelRQ2

train2 = predict(ModelRQ2, RQ2_test)
head(train2)

Matrix4 <- unlist(train2, RQ2_test$Comb16.Medicare.Beneficiaries.2016)
table(Matrix4)



