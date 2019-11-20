#Ridge, Lasso, and Elastic with GLMNET: Project 1

#Make sure the glmnet package is loaded
library(glmnet)

#Set the seed to get the same results each time
set.seed(123)

#Set my working directory
setwd("C:\\Users\\weavenic\\Documents\\Adv Stat Methods for Research\\Project 1")

# read in wine data and combine it with column for wine type (red or white)
datw <- read.table("WhiteWine.txt", sep=";", header=T)
dim(datw)
names(datw)

datr <- read.table("RedWine.txt", sep=";", header=T)
dim(datr)
names(datr)

type=c(rep("white", length(datw[,1])))
datw=cbind(datw, type)

dim(datw)
names(datw)

type =c(rep("red", length(datr[,1])))
datr =cbind(datr, type)

dim(datr)
names(datr)

dat <- rbind(datr, datw)
dim(dat)
names(dat)

#Reorder the data frame for ease
dat <- data.frame(dat[,1:10], dat$quality, dat$alcohol, dat$type)
names(dat)[13] <- "type"
names(dat)[12] <- "alcohol"
names(dat)[11] <- "quality"

# Make a training data set now with the overall data	
sample <- sample.int(n = nrow(dat), size = floor(.75*nrow(dat)), replace = F)
training <- dat[sample, ]
test.data  <- dat[-sample, ]
dim(test.data)
fix(training)

mean_train<-apply(training[, 1:11], 2, mean)
var_train<-apply(training[, 1:11], 2, var)

#Scale the training data, use it to scale the test data
training <- data.frame(scale(training[,1:11]), training$alcohol, training$type)
str(training)

##	NEED TO APPLY TRAINING MEAN AND STDEV TO STANDARDIZE TEST SET!! ##
test.std<-as.data.frame(t((t(test.data[, 1:11])-mean_train)/sqrt(var_train)))
dat.test <- data.frame(test.std, test.data$alcohol, test.data$type)
apply(test.std, 2, mean)
apply(test.std, 2, var)

names(training)[12] <- "alcohol"
names(dat.test)[12] <- "alcohol"
names(training)[13] <- "type"
names(dat.test)[13] <- "type"

names(dat.test)
str(training)

############  RIDGE  ###############
####  model  ####
y<-as.matrix(training$alcohol)
x<-as.matrix(training[,-(12:13)])  ##must make x a matrix
ans <- glmnet(x, y , alpha=0) ##alpha = 0 is ridge regression
ans

plot(ans, xlab="L2 Norm since alpha = 0")
abline(h=0, lty=2)

coef(ans, s = 5) ##where lambda is 5

### use cross-validation (we will cover more later)  ###
ans.cv <- cv.glmnet(x, y , alpha=0) ##alpha = 0 is ridge regression
plot(ans.cv)

ans.cv$lambda.min
coef(ans.cv, s = "lambda.min")

#Let us find the prediction error now for our ridge model! I honestly didn't feel like 
#looking up a way to use predict here. I'm sure there is better code for it, but this gets the job done!

l <- vector("numeric", nrow(dat.test))
for (i in 1:nrow(dat.test))
{ 
  dummy <- coef(ans.cv)[1]
  for(j in 1: 11) 
  {
    dummy <- dummy + coef(ans.cv)[j+1]*dat.test[i,j]
  } 
  l[[i]] <- (dummy -dat.test$alcohol[i])^2
}

pe.ridge <- mean(l)
pe.ridge

#Find the prediction error for red only

r <- vector("numeric", nrow(subset(dat.test, type == "red")))
for (i in 1:nrow(subset(dat.test, type == "red"))) 
{
  dummy <- coef(ans.cv)[1]
  for(j in 1:11)
  {
    dummy <- dummy + coef(ans.cv)[j+1]*subset(dat.test, type == "red")[i, j]
  }
  r[[i]] <- (dummy - subset(dat.test, type == "red")$alcohol[i])^2
}

pe.r.ridge <- mean(r)
pe.r.ridge

#Find the prediction error for white only

w <- vector("numeric", nrow(subset(dat.test, type == "white")))
for (i in 1:nrow(subset(dat.test, type == "white"))) 
{
  dummy <- coef(ans.cv)[1]
  for(j in 1:11)
  {
    dummy <- dummy + coef(ans.cv)[j+1]*subset(dat.test, type == "white")[i, j]
  }
  w[[i]] <- (dummy - subset(dat.test, type == "white")$alcohol[i])^2
}

pe.w.ridge <- mean(w)
pe.w.ridge


#Compare all three for ridge
pe.ridge
pe.r.ridge
pe.w.ridge

############  LASSO  ###############
ans.l <- glmnet(x, y , alpha=1) ##alpha = 0 is lasso regression

plot(ans.l, label=T)
abline(h=0, lty=2)
coef(ans.l, s = .1) ##where lambda is 5

### use cross-validation (we will cover more later)  ###
ans.l.cv <- cv.glmnet(x, y , alpha=1) ##alpha = 1 is lasso regression
plot(ans.l.cv)

ans.l.cv$lambda.min
coef(ans.l.cv, s = "lambda.min")


#Let us find the prediction error now for our ridge model! I honestly didn't feel like 
#looking up a way to use predict here. I'm sure there is better code for it, but this gets the job done!

l.lasso <- vector("numeric", nrow(dat.test))
for (i in 1:nrow(dat.test))
{ 
  dummy <- coef(ans.l.cv)[1]
  for(j in 1: 11) 
  {
    dummy <- dummy + coef(ans.l.cv)[j+1]*dat.test[i,j]
  } 
  l.lasso[[i]] <- (dummy -dat.test$alcohol[i])^2
}

pe.lasso <- mean(l.lasso)

#Find the prediction error for red only

r.lasso <- vector("numeric", nrow(subset(dat.test, type == "red")))
for (i in 1:nrow(subset(dat.test, type == "red"))) 
{
  dummy <- coef(ans.l.cv)[1]
  for(j in 1:11)
  {
    dummy <- dummy + coef(ans.l.cv)[j+1]*subset(dat.test, type == "red")[i, j]
  }
  r.lasso[[i]] <- (dummy - subset(dat.test, type == "red")$alcohol[i])^2
}

pe.r.lasso <- mean(r.lasso)

#Find the prediction error for white only

w.lasso <- vector("numeric", nrow(subset(dat.test, type == "white")))
for (i in 1:nrow(subset(dat.test, type == "white"))) 
{
  dummy <- coef(ans.l.cv)[1]
  for(j in 1:11)
  {
    dummy <- dummy + coef(ans.l.cv)[j+1]*subset(dat.test, type == "white")[i, j]
  }
  w.lasso[[i]] <- (dummy - subset(dat.test, type == "white")$alcohol[i])^2
}

pe.w.lasso <- mean(w.lasso)

#Here are the predicted errors for lasso:
pe.lasso
pe.r.lasso
pe.w.lasso
