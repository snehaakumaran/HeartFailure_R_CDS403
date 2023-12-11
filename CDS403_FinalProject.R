#################################################################################################################
#Name:Sneha Kumaran
#Email: skumara2@gmu.edu
#G Number: G01125110
#CDS 403 Final (SPRING 2022)
###############################################################################################################
#dataset: http://seshatdatabank.info/moralizinggodsdata/data/download.csv?

library(dplyr)

#Loading the dataset into the variable url
url<-"http://seshatdatabank.info/moralizinggodsdata/data/download.csv?"

#reading the dataset
mydata<-read.csv(url)

##### Data Preprocessing ##########
#Checking the missing or null values
is.null(mydata)

#Summary of data or checking the features
summary(mydata)


#Subsetting the dataset after checking the statistical significance of the data - these are where the numerical values are shown
mydata <- mydata[ , c(3:14 )] 
head(mydata)

#used from lecture 3 Notes- KNN
# now we create training and tes ting datasets
l<-length(row.names(mydata))

mydataset_train <- mydata[1:round(0.7*l), ] # we split the rows into 70 - 30%
mydataset_test <- mydata[round((0.7*l)+1):l, ] 
mydataset_train_labels <- mydata[1:round(0.7*l), 1] ##create labels, include in seperate vector
mydataset_test_labels <- mydata[round((0.7*l)+1):l, 1] ## same as above ^ but for testing data


# Feature Scaling -  "is the process of eliminating units of measurement for variables within a dataset, 
#and is often carried out to boost the accuracy of a machine learning algorithm." From the website https://datatricks.co.uk/feature-scaling-in-r-five-simple-methods#:~:text=Feature%20scaling%20is%20the%20process,range%20of%2050%20to%20110kg.

mydataset_train[1:11] = scale(mydataset_train[1:11])
mydataset_train
mydataset_test[1:11] = scale(mydataset_test[1:11])
mydataset_test

########### K-NN Algorithm ########################
# Fitting K-NN to the Training set and Predicting the Test set results
# Using class package to fit the data into a model
set.seed(250) # To get the same results every time we run it
library(class)
start_time <- Sys.time() #if you want to time your code run, we use sys.time
mydataset_test_pred = knn(train = mydataset_train[, -12],
             test = mydataset_test[, -12],
             cl = mydataset_train[, 12],
             k = 5, 
             prob = TRUE)
end_time <- Sys.time()  #if you want to time your code run, we end with sys.time
end_time - start_time # we subtract with end_time - start_time


# evaluating model performance
library(gmodels)
CrossTable(x = mydataset_test_labels, y = mydataset_test_pred, prop.chisq=FALSE) #creating a cross table to evaluating the model created


#Checking how well the algorithm worked so we will see the accuracy, recall, precision, and F1- score
# Making the Confusion Matrix
cm = table(mydataset_test[, 12], mydataset_test_pred)
cm
#Accuracy
Accuracy=(cm[1]+cm[4])/(cm[1]+cm[4]+cm[3]+cm[2])
Accuracy
# Recall
Recall=(cm[4])/(cm[4]+cm[2])
Recall
# Precision
Precision=(cm[4])/(cm[3]+cm[4])
Precision
#F1 Score
F1Score= 2*Precision * Recall / (Precision + Recall)
F1Score

######### K means clustering  ##################
#To get more in depth idea of how levels and money play an important role in "moralizing gods" , we are going to use 
#level and money as two important aspects in K means clustering algorithm.

#used lecture 12 notes
url<-"http://seshatdatabank.info/moralizinggodsdata/data/download.csv?"

#reading the dataset
mydata<-read.csv(url)

head(mydata)

data <-mydata[ , c("levels", "money", "MG_corr")] #categories
summary(data)

set.seed(254)
mydata <- mydata[ , c(3:14 )]
interests <- mydata

# If you recall from KNN, a common practice employed prior to any analysis using distance calculations is to normalize or z-score standardize the features so that each utilizes the same range:
interests_z <- as.data.frame(lapply(interests, scale))

# Now we run the kmeans model on 5 clusters
set.seed(2345)
mydata_clusters <- kmeans(interests_z, 5)

# Evaluating model performance:
mydata_clusters$size 
mydata_clusters$centers


# Improving model performance:
# We'll begin by applying the clusters back onto the full dataset:
mydata$cluster <- mydata_clusters$cluster

mydata[1:13, c("cluster", "levels","government", "money", "MG_corr")]

aggregate(data = mydata, levels ~ cluster, mean) 
#aggregate - levels - the means tend to vary from cluster 1-5 where it ranges from 0.8-5.9
aggregate(data = mydata, government ~ cluster, mean) 
#aggregate - government - the means tend to vary from cluster 1-5, where it ranges from 0.0-0.8
aggregate(data = mydata, money ~ cluster, mean) 
#aggregate - money -the means tend to vary from cluster 1-5, where it ranges from 0.8-4.9
aggregate(data = mydata, MG_corr ~ cluster, mean) 
#aggregate - MG_corr - the means tend to vary from cluster 1-5, where it ranges from 0.0-1.0


# For In-depth Analysis, we are taking Levels and money as two main factors
#In this, we are using the elbow method to find the optimal number of clusters
#I have used the elbow method taught from a course, so I applied my knowledge to portray a visual cluster plot.

mydata <-mydata[ , c("levels", "money", "MG_corr")] 
set.seed(254)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(mydata, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = mydata, centers = 5)
y_kmeans = kmeans$cluster

# Visualizing the clusters
library(cluster)
clusplot(mydata,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('moralizing gods'),
         xlab = 'money',
         ylab = 'levels')


#Explanation:
#Based on the results of the KNN and the K means Algorithm, there is a  clear relationship between the idea of "moralizing gods" (religion) and the idea of social complexity. 
#The variables or the category we used to see if there is a proper relationship between moralizing gods and the idea of social complexity is clearly depicted with the clusters of money and levels. We had a accuracy rate of 94.9% for the KNN algorithm. The Recall is 89.76%. Precision is 1. The F1 Score is 94.6%.
#After plotting the cluster, there are main clusters around money 1-2 and levels -1.0 - 0.8, we can see how the MG_Corr is correlated in between the clusters of money and levels.. The two components (money and levels) explain about 93.79% of the point variability. The means from K means algorith also varied in between each three variables chosen. 
#Therefore, I believe there is some sort of relationship between moralizing gods and the idea of social complexity.

