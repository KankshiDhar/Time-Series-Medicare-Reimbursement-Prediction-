#Importing the complete dataset with over 9 million rows (9714896 to be precise)
project<-read.csv(file.choose())
View(project)
project1<-project[!is.na(project$Last.Name.Organization.Name.of.the.Provider),]

#Creating a random sample from the project variable,named random_sample_project with 300,000 rows from the project dataframe
random_2013<-project[sample(nrow(project), 200000),]
View(random_2013)

#Creating a new column for percentage of amount reimbursed as part of medicare policy
#Checking datatypes of all the variables
str(random_2013)

library(tidyr)
random_2014 %>% drop_na()

#Converting the factors to numeric
sum(is.na(random_2014$Average.Medicare.Allowed.Amount))

random_2013$AVERAGE_MEDICARE_ALLOWED_AMT<-as.numeric(as.character(random_2013$AVERAGE_MEDICARE_ALLOWED_AMT))
random_2013$AVERAGE_SUBMITTED_CHRG_AMT<-as.numeric(as.character(random_2013$AVERAGE_SUBMITTED_CHRG_AMT))
random_2013$AVERAGE_MEDICARE_PAYMENT_AMT<-as.numeric(as.character(random_2013$AVERAGE_MEDICARE_PAYMENT_AMT))

#Creating the column percent_reimbursed
random_2013$percent_reimbursed <- (random_2013$AVERAGE_MEDICARE_PAYMENT_AMT/random_2013$AVERAGE_SUBMITTED_CHRG_AMT)*100
View(random_2013$percent_reimbursed)

#Subset of the most common provider types

CommonProviders_2013 <-subset(random_2013, Provider.Type == "Family Practice" | Provider.Type == "Physician Assistant" | Provider.Type == "Nurse Practitioner" | Provider.Type == "Internal Medicine" | Provider.Type == "General Medicine")
View(CommonProviders_2013)

#Adding a new column for the year 2014
CommonProviders_2013$Year <- rep("2013", length(CommonProviders_2013$Provider.Type))


#Extracting to a csv file
write.table(CommonProviders_2013, file="2013.csv", row.names = F, sep = ",")
