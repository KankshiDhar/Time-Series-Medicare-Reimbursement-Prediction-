#RESEARCH QUESTION#1
#Importing the 2016 Medicare Provider and Utilization dataset into medicare
medicare <- read.csv(file.choose())
View(medicare)
attach(medicare)

#Creating the testing and training data
medicare.train = medicare[1:48536, ] # about 80%
medicare.test  = medicare[48537:60671, ] # about 20%

View(medicare.train)

#Linear Regression
#1:Gender of the provider

plot(medicare.train$percent_reimbursed, medicare.train$Gender.of.the.Provider, pch = 16)
fit.lm1 <-lm(percent_reimbursed ~ Gender.of.the.Provider, medicare.train)
fit.lm1

#Line of fit for the gender of the provider
abline(fit.lm1, col="Red", lwd=3)

#Computing the fitted and residual values
fitted(fit.lm1)
residuals(fit.lm1)

qqnorm(residuals(fit.lm1))
qqline(residuals(fit.lm1), col = "Blue")

#Using trained model to predict testing data
p<- predict(fit.lm1, newdata = medicare.test)
p
prediction1<-data.frame(medicare.test$Gender.of.the.Provider, p)
View(prediction1)

#Confidence and Prediction bands
pc<- predict(fit.lm1, int = "c", newdata = medicare.test)
pp<- predict(fit.lm1, int = "p", newdata = medicare.test)
plot(medicare.test$percent_reimbursed, medicare.test$Gender.of.the.Provider)
library(car)
View(medicare.test)
View(p)
matlines(medicare.test, pc)
matlines(medicare.test, pp)

#accuracy
prediction<-predict(fit.lm1, newdata = medicare.test)
cor(prediction, medicare.test$percent_reimbursed)
rmse(fit.lm1$fitted.values, medicare.test$percent_reimbursed)

#2: Linear Regression for Provider type

fit.lm2<- lm(percent_reimbursed ~ Provider.Type, medicare.train)
fit.lm2

#Computing the fitted and residual values
fitted(fit.lm2)
residuals(fit.lm2)

qqnorm(residuals(fit.lm2))
qqline(residuals(fit.lm2), col = "Blue")

#Using the trained model to predict the testing data
p<- predict(fit.lm2, newdata = medicare.test)
p
prediction1<-data.frame(medicare.test$Provider.Type, p)
View(prediction1)

#Testing the accuracy of the model
prediction<-predict(fit.lm2, newdata = medicare.test)
cor(prediction, medicare.test$percent_reimbursed)
rmse(fit.lm2$fitted.values, medicare.test$percent_reimbursed)

# 3. Medicare Participatation Indicator

plot(percent_reimbursed, Medicare.Participation.Indicator)

fit.lm3<-lm(percent_reimbursed ~ Medicare.Participation.Indicator, medicare.train)
fit.lm3

#Computing the fitted and residual values
fitted(fit.lm3)
residuals(fit.lm3)

qqnorm(residuals(fit.lm3))
qqline(residuals(fit.lm3), col = "Blue")

#Using trained model to predict testing data
p<- predict(fit.lm3, newdata = medicare.test)
p
prediction1<-data.frame(medicare.test$Medicare.Participation.Indicator, p)
View(prediction1)

#accuracy
prediction<-predict(fit.lm3, newdata = medicare.test)
cor(prediction, medicare.test$percent_reimbursed)
rmse(fit.lm3$fitted.values, medicare.test$percent_reimbursed)

# 4: Number of Services

plot(percent_reimbursed, Number.of.Services, col = "Red", xlab = "Percent Reimbursement Amount", ylab = "Number of Servies", main = "Plot of Percent Reimbursement vs Number of Services")

fit.lm4 <- lm(percent_reimbursed ~ Number.of.Services, medicare.train)
fit.lm4

abline(fit.lm4)

#Computing the fitted and residual values
fitted(fit.lm4)
residuals(fit.lm4)

qqnorm(residuals(fit.lm4))
qqline(residuals(fit.lm4), col = "Blue")

#Using trained model to predict testing data
p<- predict(fit.lm4, newdata = medicare.test)
p
prediction1<-data.frame(medicare.test$Number.of.Services, p)
View(prediction1)

#Testing the accuracy of the model
prediction<-predict(fit.lm4, newdata = medicare.test)
cor(prediction, medicare.test$percent_reimbursed)
rmse(fit.lm4$fitted.values, medicare.test$percent_reimbursed)

# 5: Number of Medicare Beneficiaries
plot(percent_reimbursed , Number.of.Medicare.Beneficiaries, cex.main = 1,col = "Red",main = "Percent Reimbursement Amount based on Number of Medicare Beneficiaries", xlab = "Number of Medicare Beneficiaries", ylab = "Percent Reimbursement Amount")

fit.lm5 <-lm(percent_reimbursed ~ Number.of.Medicare.Beneficiaries, medicare.train)
fit.lm5

#Line of fit for the gender of the provider
abline(fit.lm5, col="Blue", lwd=3)

#Computing the fitted and residual values
fitted(fit.lm5)
residuals(fit.lm5)

qqnorm(residuals(fit.lm5))
qqline(residuals(fit.lm5), col = "Blue")

#Using trained model to predict testing data
p<- predict(fit.lm5, newdata = medicare.test)
p
prediction1<-data.frame(medicare.test$Number.of.Medicare.Beneficiaries, p)
View(prediction1)

#accuracy
prediction<-predict(fit.lm5, newdata = medicare.test)
cor(prediction, medicare.test$percent_reimbursed)
rmse(fit.lm5$fitted.values, medicare.test$percent_reimbursed)

#Regression model based on Average Medicare Allowed Amount

plot(percent_reimbursed , Average.Medicare.Allowed.Amount, cex.main = 1,col = "Red",main = "Percent Reimbursement Amount based on Avg Medicare Allowed Amount", xlab = "Number of Medicare Beneficiaries", ylab = "Percent Reimbursement Amount")

fit.lm6 <-lm(percent_reimbursed ~ Average.Medicare.Allowed.Amount, medicare.train)
fit.lm6

#Line of fit for the gender of the provider
abline(fit.lm6, col="Blue", lwd=3)

#Computing the fitted and residual values
fitted(fit.lm6)
residuals(fit.lm6)

qqnorm(residuals(fit.lm6))
qqline(residuals(fit.lm6), col = "Blue")

#Using trained model to predict testing data
p<- predict(fit.lm6, newdata = medicare.test)
p
prediction1<-data.frame(medicare.test$Average.Medicare.Allowed.Amount, p)
View(prediction1)

#accuracy
prediction<-predict(fit.lm6, newdata = medicare.test)
cor(prediction, medicare.test$percent_reimbursed)
rmse(fit.lm6$fitted.values, medicare.test$percent_reimbursed)

#Regression model based on Average Submitted Charge Amount

plot(percent_reimbursed , Average.Submitted.Charge.Amount, cex.main = 1,col = "Red",main = "Percent Reimbursement Amount based on Avg Submitted Charge Amount", xlab = "Number of Medicare Beneficiaries", ylab = "Percent Reimbursement Amount")

fit.lm7 <-lm(percent_reimbursed ~ Average.Submitted.Charge.Amount , medicare.train)
fit.lm7

#Line of fit for the gender of the provider
abline(fit.lm7, col="Blue", lwd=3)

#Computing the fitted and residual values
fitted(fit.lm7)
residuals(fit.lm7)

qqnorm(residuals(fit.lm7))
qqline(residuals(fit.lm7), col = "Blue")

#Using trained model to predict testing data
p<- predict(fit.lm7, newdata = medicare.test)
p
prediction1<-data.frame(medicare.test$Average.Submitted.Charge.Amount, p)
View(prediction1)

#accuracy
prediction<-predict(fit.lm7, newdata = medicare.test)
cor(prediction, medicare.test$percent_reimbursed)
rmse(fit.lm6$fitted.values, medicare.test$percent_reimbursed)

#MULTIVARIATE REGRESSION
#Before we can proceed with multivariate regression with differennt combinations of variables, 
#we run a regression model with a combination of five features in the dataset as predictors and
#outcome as the percent_reimbursed

medicare.train$gender <- ifelse(medicare.train$Gender.of.the.Provider == "M", 1,0)

#Multivariate Regression combination 1

model1<-lm(percent_reimbursed ~ Gender.of.the.Provider + Medicare.Participation.Indicator + Number.of.Medicare.Beneficiaries + Average.Medicare.Allowed.Amount + Number.of.Services)
model1

#Combination 2: 
#Creating a dataframe with all the numeric predictors

medicare2<- data.frame(medicare.train$Average.Medicare.Allowed.Amount, medicare.train$Average.Submitted.Charge.Amount, medicare.train$Average.Medicare.Payment.Amount, medicare.train$Average.Medicare.Standardized.Amount, medicare.train$Number.of.Services, medicare.train$Number.of.Medicare.Beneficiaries, medicare.train$percent_reimbursed, medicare.train$Gender.of.the.Provider)
View(medicare2)

#Multivariate regression model with the second combination of numeric variables

model <- lm(percent_reimbursed ~  Average.Medicare.Allowed.Amount+ Average.Submitted.Charge.Amount + Average.Medicare.Standardized.Amount + Average.Medicare.Payment.Amount + Number.of.Services + Number.of.Medicare.Beneficiaries)
model

#Using the above model to predict the output of test data
prediction <- predict(model, newdata = medicare.test, type = "response")
cor(prediction, medicare.test$percent_reimbursed)

install.packages("Metrics")
library(Metrics)
rmse(model$fitted.values, medicare.test$percent_reimbursed)

#REGULARIZATION
install.packages("glmnet")
library(glmnet)

cv.fit <- cv.glmnet(as.matrix(medicare2[,c(-7,-8)]), as.vector(medicare2[,7]), alpha = 1)
plot(cv.fit)
coef(cv.fit)

#Predicting the accuracy
prediction <- predict(cv.fit, newx = as.matrix(medicare2[,c(-7,-8)]))

cor(prediction, as.vector(medicare2[,7]))

#RESEARCH QUESTION#2:
install.packages("xlsx")
library(xlsx)
rq2 <- read.csv(file.choose())
View(rq2)

#Linear regression model for the number of medicare beneficiaries in 2016 based on the percent reimbursement 
#rate from 2014

lm1 <- lm(rq2$Medicare.Beneficiaries.2016 ~ rq2$percent.2014)
lm1

plot(rq2$Medicare.Beneficiaries.2016 , rq2$percent.2014, xlab = "# of Beneficiaries in 2016", ylab = "2014 reimbursement Rate", main = "2014 Reimbursement Rate vs 2016 count of Beneficiaries", col = "Darkgreen")
abline(lm1)

#Computing the fitted and residual values
fitted(lm1)
residuals(lm1)

qqnorm(residuals(lm1))
qqline(residuals(lm1), col = "Blue")

#Using the trained model to predict the testing data
p<- predict(lm1, newdata = rq2)
p
prediction1<-data.frame(rq2$percent.2014, p)
View(prediction1)

#Testing the accuracy of the model
prediction<-predict(lm1, newdata = rq2)
cor(prediction, rq2$Medicare.Beneficiaries.2016)
rmse(lm1$fitted.values,rq2$Medicare.Beneficiaries.2016 )







