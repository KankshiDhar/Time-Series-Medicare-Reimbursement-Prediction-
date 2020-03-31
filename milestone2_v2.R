medicare_data <- read.csv(file.choose())
medicare_data$Reimburse <- ifelse(medicare_data$percent_reimbursed > 90, 1,0)

medicare_data$Gender.of.the.Provider = factor(medicare_data$Gender.of.the.Provider)
medicare_data$Provider.Type.of.the.Provider = factor(medicare_data$Provider.Type.of.the.Provider)
medicare_data$Provider.Type = factor(medicare_data$Provider.Type)
medicare_data$Medicare.Participation.Indicator = factor(medicare_data$Medicare.Participation.Indicator)

random_medicare_data.train = random_medicare_data[1:120000, ]
random_medicare_data.test = random_medicare_data[120001:178495,]

medicare_dtree <- rpart(Reimburse~Gender.of.the.Provider+Entity.Type.of.the.Provider+Provider.Type.of.the.Provider+Medicare.Participation.Indicator+HCPCS.Code+Number.of.Services+Number.of.Medicare.Beneficiaries+Average.Medicare.Allowed.Amount+Average.Submitted.Charge.Amount,data=medicare_data.train,method="class")
medicare_test_Pred <- predict(medicare_dtree, medicare_data.test, type = 'class')
table(medicare_test_Pred, medicare_data.test$Reimburse)
medicare_train_Pred <- predict(medicare_dtree, medicare_data.train, type = 'class')
table(medicare_train_Pred, medicare_data.train$Reimburse)

#Work on RQ2
Combined$lost_service <- ifelse(Combined$Medicare.Beneficiaries.2016 > Combined$Medicare.Beneficiaries.2014 , 0,1)
Combined.train = Combined[1:900, ]
Combined.test = Combined[901:1176, ]
RQ2_dtree <- rpart(lost_service~Percent.2014 + Provider.Type,data=Combined.train,method="class")
lost_service_Pred <- predict(RQ2_dtree, Combined.train, type = 'class')
table(lost_service_Pred, Combined.train$lost_service)
lost_service_Pred_test <- predict(RQ2_dtree, Combined.test, type = 'class')
table(lost_service_Pred_test, Combined.test$lost_service)