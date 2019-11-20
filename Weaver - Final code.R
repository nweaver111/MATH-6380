#Nicholas Weaver
#Final Project - Stat 6388 
#Spectral Clustering

#The goal of this code is to go through spectral clutsering, step-by-step,
# to get a better understanding of how the algorithm is implemented!

###### Motivation #######
#I will use the spirals data set to get a motivational example.
library(kernlab)

data(spirals)

par(mfrow = c(1,2))
plot(spirals, xlab = "X", ylab = "Y", main = "Spirals Data Set")

#Use a k-means algorithm to cluster the data poorly
km <- kmeans(spirals, centers = 2, nstart = 20)
plot(spirals, xlab = "X", ylab = "Y", main = "K-means Clustering",
     col = km$cluster+2)

# We use the above image to show the need for spectral clustering


##### Data Exploration ######

dat1 <- read.csv("C:\\Users\\Nicholas\\Documents\\STAT 6388\\Final Project\\data.txt", 
                 header = FALSE, sep = "")
dat1

#Give a name to the columns
colnames(dat1) <- c("Area", "Perimeter", "Compactness",
                    "length","width","asymmetry", "groove", "type")


#Split data into test and train datasets based upon previous code used in the course
set.seed(123)
a <- 0.7*nrow(dat1)
summary(dat1)
tmp.random<-sample(1:nrow(dat1))
training<-dat1[tmp.random[1:a],]
training

#Explore the data

summary(training)

#Standardize the data
train <- data.frame(scale(training[1:7]), training$type)
train
mean_train<-apply(training[1:7], 2, mean)
var_train<-apply(training[1:7], 2, var)

apply(train, 2, mean)
apply(train, 2, var)

#New summary table
summary(train)

#Make a correlation matrix to see how correlated the features appear to be.

corr <- round(cor(train[,1:7]), 2)
ggcorrplot(corr, type="lower", lab=TRUE)

#Recall that correlation isn't a bad thing here. We want to group our values by 
#Something, so this is good!

#Bivariate plots. Colors are given to see how separated the data is before even running
# a clustering technique.

plot(train$groove ~ train$Perimeter, xlab = "Perimeter", 
     ylab = "Groove", main = "Area vs. Compactness", col = train$training.type+2)
plot(train$length ~ train$groove, xlab = "Lenght of Groove", 
     ylab = "Length of Kernel", main = "Kernel length vs. Groove length", 
     col = train$training.type+2)
plot(train$asymmetry ~ train$width, xlab = "Width of Kernel", 
     ylab = "Asymmetry", main = "Asymmetry vs. Width", col = train$training.type+2)
plot(train$Perimeter ~ train$asymmetry, xlab = "Asymmetry", 
     ylab = "Perimeter", main = "Perimeter vs Asymmetry", col = train$training.type+2)

# It is clear that the data already seems to be nicely separated by these covariates. Thus
# we would expect spectral clustering to be an overkill, but it can still be applied to this
# data to see how it compares to k-means when both are applicable!


##### Spectral Clustering code ######

#Create the similarity matrix. We will use four different similarity measurements. Note
# that this code isn't as nice as it could be. For instance, you can only select one of these
# functions below to create at a time. In retrospect, it would make more sense to simply
# give them different names and loop through using them...

# Gaussian
s <- function(x1, x2, alpha=1) {
  exp(- alpha * norm(as.matrix(x1-x2), type="F"))
}

# Correlation
s <- function(x1, x2, alpha=1){
  crossprod(x1, x2)/sqrt(crossprod(x1)*crossprod(x2))
}

# Geodesic
s <- function(x1, x2, alpha=1){
  exp(-acos(crossprod(x1, x2)/sqrt(crossprod(x1)*crossprod(x2))))
}

# Manhattan
s <- function(x1, x2, alpha = 1){
  exp(-sum(abs(x1 - x2)))
}

# Given the similairty measurement, we want to create the square matrix for all of the 
# pairwise similarities.

make.similarity <- function(my.data, similarity) {
  N <- nrow(my.data)
  S <- matrix(rep(NA,N^2), ncol=N)
  for(i in 1:N) {
    for(j in 1:N) {
      S[i,j] <- similarity(my.data[i,], my.data[j,])
    }
  }
  S
}

S <- make.similarity(my.data, s)
S[1:8,1:8]

# now we want to make the similarity matrix sparse (also called the affinity matrix).
# we will again create a function to accomplish this goal. Notice that the function will
# ensure the graph is symmetric (and also save time by not running through 
# all possible values).
# 
# Finally, the function is designed to give a k-nearest neighbors approach. 
# A different function would need to be created to get a mutual k-nearest neighbors
# or an epsilon neighborhood.

make.affinity <- function(S, n.neighboors=2) {
  N <- length(S[,1])
  
  if (n.neighboors >= N) {  # fully connected
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  A  
}

# The benefits of using a function to create the affinity matrix is that we can easily
# change the number of neighbors we want to use!

A <- make.affinity(S, 10)  # use 10 neighboors (includes self)
A[1:8,1:8]

#Create the diagonal matrix in preparation for the graph Laplacian.
D <- diag(apply(A, 1, sum)+1) # sum rows
D[1:8,1:8]

# Create the unnormalized graph Laplacian (U stands for unnormalized)
U <- D - A
round(U[1:12,1:12],1)

#Now create the eigenvector matrix to run the k-means algorithm on:
k   <- 3
evL <- eigen(U, symmetric=TRUE)
Z   <- evL$vectors[,(ncol(evL$vectors)-k+1):ncol(evL$vectors)]

# Plotting the eigenvectors helps to see/ make sure the data is being split
# into convex clusters.
plot(Z, col=train$training.type+2,  pch=19, xlab = "smallest Eigenvector", 
     ylab = "Second smallest Eigenvector", main = "Eigenvector plot") 

#make the graph look nicer
grid(nx=)

#Run a k-means on the new matrix
library(stats)
km <- kmeans(Z, centers=k, nstart=20)
plot(1:nrow(train) ~train$training.type, pch = 19, col=km$cluster+2, 
     xlab = "Wheat type", ylab = "Index")


#Create a eigengap plot to ensure choice of k is reasonable.
plot(sort(evL$values, decreasing = FALSE)[1:15], col = 3,
     ylab = "Value", xlab = "Ordered Eigenvalues", pch = 19, 
     main = "15 Smallest Eigenvalues")

# make it look nice
grid(nx=)

#give a visual for a cut-off point
abline(.5,0)

#Make a table to determine accuracy of the model!
table(train$training.type, km$cluster, dnn = c("True Type", "Cluster Assignment"))

#What would a normal kmeans provide? It is useful to compare this result to the method that
# most of the class already knowns!

kmreg <- kmeans(train[,1:7], centers = k, nstart = 20)

#show accuracy in a table:
table(train$training.type, kmreg$cluster, dnn = c("True Type", "Cluster Assignment"))

#They were the same! So we could have done just as well with normal kmeans clustering in this
# example! 