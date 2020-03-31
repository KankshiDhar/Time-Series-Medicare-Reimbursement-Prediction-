#Importing the complete dataset with over 9 million rows (9714896 to be precise)
project<-read.csv(file.choose())
View(project)
project1<-project[!is.na(project$Last.Name.Organization.Name.of.the.Provider),]

#Creating a random sample from the project variable,named random_sample_project with 300,000 rows from the project dataframe
random_sample_project<-project[sample(nrow(project), 200000),]
View(random_sample_project)

#Creating a new column for percentage of amount reimbursed as part of medicare policy
random_sample_project$percent_reimbursed <- (random_sample_project$Average.Medicare.Payment.Amount/random_sample_project$Average.Submitted.Charge.Amount)*100
random_sample_project$percent_reimbursed

View(random_sample_project)

#Subset of the most common provider types
CommonProviders <-subset(random_sample_project, Provider.Type == "Family Practice" | Provider.Type == "Physician Assistant" | Provider.Type == "Nurse Practitioner" | Provider.Type == "Internal Medicine" | Provider.Type == "General Medicine")
View(CommonProviders)

CommonProviders$Year <- rep("2016", 60671)


#Extracting to a csv file
write.table(CommonProviders, file="2016.csv", row.names = F, sep = ",")