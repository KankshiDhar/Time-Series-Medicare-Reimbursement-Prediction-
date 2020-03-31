#Importing the complete dataset with over 9 million rows (9714896 to be precise)
project<-read.csv(file.choose())
View(project)
project1<-project[!is.na(project$Last.Name.Organization.Name.of.the.Provider),]

#Creating a random sample from the project variable,named random_sample_project with 300,000 rows from the project dataframe
random_sample_project<-project[sample(nrow(project), 300000),]
View(random_sample_project)

#Complete cases in case we want to include only the complete rows for future analysis
randm2<-random_sample_project
randm2[randm2==" "]<-NA
randm2<-randm2[complete.cases(randm2),]
randm2


#For our analysis, we are checking if blank values exist in the rows that have data about the 
#Medicare amounts. Any blank values, if present, are substituted with NA, and we are finding
#the sum of NA values. There are no blank/NA values in those columns

Remove_blank_NA<-random_sample_project
Remove_blank_NA

#For the variable Average Medicare Payment Amount
Remove_blank_NA$Average.Medicare.Payment.Amount<-ifelse(Remove_blank_NA$Average.Medicare.Payment.Amount==" ", NA, Remove_blank_NA$Average.Medicare.Payment.Amount)
sum(is.na(Remove_blank_NA$Average.Medicare.Payment.Amount))

#For the variable Average Medicare Allowed Amount
Remove_blank_NA$Average.Medicare.Allowed.Amount<-ifelse(Remove_blank_NA$Average.Medicare.Allowed.Amount==" ", NA, Remove_blank_NA$Average.Medicare.Allowed.Amount)
sum(is.na(Remove_blank_NA$Average.Medicare.Allowed.Amount))

#For the variable Average Submitted Charge Amount
Remove_blank_NA$Average.Submitted.Charge.Amount<-ifelse(Remove_blank_NA$Average.Submitted.Charge.Amount==" ", NA, Remove_blank_NA$Average.Submitted.Charge.Amount)
sum(is.na(Remove_blank_NA$Average.Submitted.Charge.Amount))

#For the variable Average Medicare Standardized Amount
Remove_blank_NA$Average.Medicare.Standardized.Amount<-ifelse(Remove_blank_NA$Average.Medicare.Standardized.Amount==" ", NA, Remove_blank_NA$Average.Medicare.Standardized.Amount)
sum(is.na(Remove_blank_NA$Average.Medicare.Standardized.Amount))

#For the variable Number of Services
Remove_blank_NA$Number.of.Services<-ifelse(Remove_blank_NA$Number.of.Services==" ", NA, Remove_blank_NA$Number.of.Services)
sum(is.na(Remove_blank_NA$Number.of.Services))


#Creating a new column for percentage of amount reimbursed as part of medicare policy
random_sample_project$percent_reimbursed <- (random_sample_project$Average.Medicare.Payment.Amount/random_sample_project$Average.Submitted.Charge.Amount)*100
random_sample_project$percent_reimbursed

#Multiple Regression to analyse the predictors for the variable percent_reimbursed
#Multiple regression for the first seven predictors
Influencing_factors<-lm(percent_reimbursed~ Average.Medicare.Allowed.Amount + Average.Submitted.Charge.Amount + Average.Medicare.Standardized.Amount + Country.Code.of.the.Provider + Number.of.Services +Gender.of.the.Provider , data = random_sample_project)
Influencing_factors
summary(Influencing_factors)

#Multiple regression for the last 5 predictors
Influencing_factors1<-lm(percent_reimbursed~ Credentials.of.the.Provider + Entity.Type.of.the.Provider + Zip.Code.of.the.Provider + HCPCS.Code + Number.of.Medicare.Beneficiaries, data = random_sample_project)
Influencing_factors1
summary(Influencing_factors1)

reg1<-lm(percent_reimbursed ~  Zip.Code.of.the.Provider + HCPCS.Code + Number.of.Medicare.Beneficiaries, data = random_sample_project)
reg1
summary(reg1)

class(random_sample_project$Credentials.of.the.Provider)
class(random_sample_project$Entity.Type.of.the.Provider)

#summary statistics for the percent reimbursed amount
summarytools::descr(random_sample_project$precent_reimbursed)
install.packages("summarytools")
summary(random_sample_project$precent_reimbursed)

#Outliers' detection and removal
random_sample_project_nooutlier <- subset(random_sample_project,precent_reimbursed >= 100)
View(random_sample_project_nooutlier)

#Finding provider types who offer 100% reimbursement
random_sample_project_nooutlier<-subset(random_sample_project,precent_reimbursed =100)

providers<-subset(random_sample_project_nooutlier, random_sample_project_nooutlier$precent_reimbursed = 100)
View(providers)
providers$Provider.Type


#Gender disparity in medicare
#Using two-sample t-test to test the gender disparity

#Creating a subset with female providers
levels(random_sample_project$Gender.of.the.Provider)

Female_providers <- subset(random_sample_project,Gender.of.the.Provider=="F")
Female_providers

levels(random_sample_project_nooutlier$Country.Code.of.the.Provider)

#Creating a subset with male providers
Male_providers<- subset(random_sample_project, Gender.of.the.Provider=="M")
Male_providers

#As males far outnumber females, creating a subset with equal number of males
Male_providers1<-Male_providers[sample(nrow(Male_providers), 83853),]
Male_providers1

#2-Sample t-test
t.test(Female_providers$precent_reimbursed, Male_providers1$precent_reimbursed, alternative = "greater", var.equal = FALSE)
t.test(Female_providers$precent_reimbursed, Male_providers1$precent_reimbursed, alternative = "two.sided", var.equal = FALSE)

