#Importing the complete dataset with over 9 million rows (9714896 to be precise)
project<-read.csv(file.choose())
View(project)
project1<-project[!is.na(project$Last.Name.Organization.Name.of.the.Provider),]

#Creating a random sample from the project variable,named random_sample_project with 300,000 rows from the project dataframe
random_2014<-project[sample(nrow(project), 200000),]
View(random_2014)

#Creating a new column for percentage of amount reimbursed as part of medicare policy
#Checking datatypes of all the variables
str(random_2014)

library(tidyr)
random_2014 %>% drop_na()

#Converting the factors to numeric
sum(is.na(random_2014$Average.Medicare.Allowed.Amount))
random_2014$Average.Medicare.Allowed.Amount<-as.numeric(as.character(random_2014$Average.Medicare.Allowed.Amount))
random_2014$Average.Submitted.Charge.Amount<-as.numeric(as.character(random_2014$Average.Submitted.Charge.Amount))
random_2014$Average.Medicare.Payment.Amount<-as.numeric(as.character(random_2014$Average.Medicare.Payment.Amount))
random_2014$Average.Medicare.Standardized.Amount<-as.numeric(as.character(random_2014$Average.Medicare.Standardized.Amount))


#Creating the column percent_reimbursed
random_2014$percent_reimbursed <- (random_2014$Average.Medicare.Payment.Amount/random_2014$Average.Submitted.Charge.Amount)*100
random_2014$percent_reimbursed

View(random_2014$percent_reimbursed)

#Subset of the most common provider types

CommonProviders_2014 <-subset(random_2014, Provider.Type.of.the.Provider == "Family Practice" | Provider.Type.of.the.Provider == "Physician Assistant" | Provider.Type.of.the.Provider == "Nurse Practitioner" | Provider.Type.of.the.Provider == "Internal Medicine" | Provider.Type.of.the.Provider == "General Medicine")
View(CommonProviders_2014)

#Adding a new column for the year 2014
CommonProviders_2014$Year <- rep("2014", length(CommonProviders_2014$Provider.Type.of.the.Provider))


#Extracting to a csv file
write.table(CommonProviders_2014, file="2014.csv", row.names = F, sep = ",")
