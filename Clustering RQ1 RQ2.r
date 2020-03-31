#INTRODUCTION TO DATA SCIENCE
#Milestone 3
#K-means method of clustering
#RESEARCH QUESTION#1

#K-means method of clustering

data1 <- read.csv(file.choose(), header = T)
View(data1)
Medicare1 <- data.frame(data1$Number.of.Services, data1$Number.of.Medicare.Beneficiaries,   data1$percent_reimbursed,Average.Medicare.Allowed.Amount, Average.Medicare.Payment.Amount)
View(Medicare1)


#Standardizing/Normalizing the columns
Medicare1.stand <- scale(Medicare1)
View(Medicare1.stand)

#elbow method to determine the number of clusters
wss <- sapply(1:15,function(k){kmeans(Medicare1.stand, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

plot(1:15, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#K-means code
trial <- kmeans(Medicare1.stand,4)
trial$centers
trial$cluster

table(data1$Provider.Type, trial$cluster)



#Plotting the clusters
plot(Medicare1.stand[trial$cluster == 1,],
     col = "red",
     xlim = c( min(Medicare1.stand[,1] ), max(Medicare1.stand[,1])),
     ylim = c( min(Medicare1.stand[,2] ), max(Medicare1.stand[,2])), main = "K-means Clustering" ,xlab = "Number of Medicare Beneficiaries", ylab = "Percent Reimbursed")
points(Medicare1.stand[trial$cluster == 2, ],
       col = "blue")
points(Medicare1.stand[trial$cluster == 3, ],
       col = "seagreen")
points(Medicare1.stand[trial$cluster == 4, ],
       col = "yellow")
points(Medicare1.stand[trial$cluster == 5, ],
       col = "purple")
points(trial$centers, pch=2, col = "green")


#RESEARCH QUESTION 2
data4 <- read.csv(file.choose())

#For research question 2, we have the percent_reimbursement in 2014 as predictor and number of beneficiaries in 2016 as outcome
Medicare4 <- data.frame(data4$Medicare.Beneficiaries.2016, data4$percent.2014)
View(Medicare4)

#Standardizing/Normalizing the columns
Medicare4.stand <- scale(Medicare4)
View(Medicare4.stand)

#elbow method to determine the number of clusters
wss <- sapply(1:15,function(k){kmeans(Medicare5.stand, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

plot(1:15, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#K-means code
trial <- kmeans(Medicare4.stand,3)
trial$centers
trial$cluster

table(data4$Provider.Type, trial$cluster)



#Plotting the clusters
plot(Medicare4.stand[trial$cluster == 1,],
     col = "red",
     xlim = c( min(Medicare4.stand[,1] ), max(Medicare4.stand[,1])),
     ylim = c( min(Medicare4.stand[,2] ), max(Medicare4.stand[,2])), main = "K-means Clustering" ,xlab = "Number of Medicare Beneficiaries", ylab = "Percent Reimbursed")
points(Medicare4.stand[trial$cluster == 2, ],
       col = "blue")
points(Medicare4.stand[trial$cluster == 3, ],
       col = "seagreen")
points(trial$centers, pch=2, col = "green")


#HIERARCHICAL METHOD OF CLUSTERING

#Hierarchical Clustering for Research Question 1
data2 <- read.csv(file.choose(), header = T)
View(data1)

#Creating a subset to include the relevant numeric variables
Medicare2 <- data.frame(data2$Number.of.Services, data2$Number.of.Medicare.Beneficiaries,   data2$percent_reimbursed, data2$Average.Medicare.Allowed.Amount, data2$Average.Medicare.Payment.Amount)
View(Medicare2)

#Scaling the dataset  for standardization
Medicare2.stand <- scale(Medicare2[-1])

#Creating a subset of 5000 data points 
random<-Medicare2.stand[sample(nrow(Medicare2.stand), 5000),]

#Computing the distnace matrix
Medi2.dist <- dist(random, method = "euclidian")

#Performing the Hierarchical Clustering
medicare.hclust <- hclust(Medi2.dist, method = "complete", members = NULL)
plot(medicare.hclust, cex = 0.6, col = "blue")

#Upper tree cut at height = 2
hcd <- as.dendrogram(medicare.hclust)

plot(cut(hcd, h=2)$upper, 
     main="Upper tree of cut at h=2")
plot(cut(hcd, h=2)$lower[[2]], 
     main="Second branch of lower tree with cut at h=2")

#Performing the hclust again with 200 rows only

random<-Medicare2.stand[sample(nrow(Medicare2.stand), 200),]

#Computing the distnace matrix
Medi2.dist <- dist(random, method = "euclidian")

#Performing the Hierarchical Clustering
medicare.hclust <- hclust(Medi2.dist, method = "complete", members = NULL)
plot(medicare.hclust, cex = 0.6, col = "blue")

# Open a PDF for plotting; units are inches by default
pdf("C:/Users/Monalisa Swami/Desktop/Final dataset/file.pdf", width=40, height=15)

# Do some plotting
plot(medicare.hclust, cex = 0.6)

# Close the PDF file's associated graphics device (necessary to finalize the output)
dev.off()


#Hierarchical Clustering for Research Question 2
data5 <- read.csv(file.choose(), header = T)
View(data5)

#Subsetting to include only the relevant variables
Medicare5 <- data.frame(data5$Medicare.Beneficiaries.2016, data5$percent.2014)
View(Medicare5)

#Standardizing/Normalizing the columns
Medicare5.stand <- scale(Medicare5)
View(Medicare5.stand)


#Computing the distnace matrix
Medi5.dist <- dist(Medicare5.stand, method = "euclidian")

#Performing the Hierarchical Clustering
medicare.hclust <- hclust(Medi5.dist, method = "complete", members = NULL)
plot(medicare.hclust, cex = 0.6, col = "blue")

#Upper tree cut at height = 2
hcd <- as.dendrogram(medicare.hclust)

plot(cut(hcd, h=2)$upper, 
     main="Upper tree of cut at h=2")


#DBSCAN METHOD OF CLUSTERING
#Density based clustering for Research Question 1

install.packages("dbscan")
library(dbscan)

data3 <- read.csv(file.choose(), header = T)
View(data3)
Medicare3 <- data.frame(data3$Number.of.Services, data3$Number.of.Medicare.Beneficiaries,   data3$percent_reimbursed, data3$Average.Medicare.Allowed.Amount, data3$Average.Medicare.Payment.Amount)
View(Medicare3)

#Standardizing/Normalizing the columns
Medicare3.stand <- scale(Medicare3[-1])
View(Medicare3.stand)

#Determining the eps
kNNdistplot(Medicare3.stand, k = 5)

#Running the DBSCAN algorithm
db = dbscan(Medicare3.stand, eps = 0.15, minPts = 3)
plot(Medicare3.stand, col = db$cluster+1, main = "DBSCAN", xlab = "Number of Medicare Beneficiaries", ylab = "Percent Reimbursed")
plot(Medicare3.stand, col = db$cluster+1)

#Density based approach for Research Question 2
data6 <- read.csv(file.choose(), header = T)
View(data6)
Medicare6 <- data.frame(data6$percent.2014, data6$Medicare.Beneficiaries.2016)
View(Medicare6)

#Standardizing/Normalizing the columns
Medicare6.stand <- scale(Medicare6)
View(Medicare6.stand)

#Determining the eps
kNNdistplot(Medicare6.stand, k = 5)

#Running the DBSCAN algorithm
db = dbscan(Medicare6.stand, eps = 0.15, minPts = 3)
plot(Medicare6.stand, col = db$cluster+1, main = "DBSCAN", xlab = "Number of Medicare Beneficiaries", ylab = "Percent Reimbursed")









