# setwd("C:/Users/Lu/Dropbox/Machine Learning/Project 3")
set.seed(0)


n <- 100
type <- c(rep('A',n/2),rep('B',n/2))
x <- c(rnorm(n/2, mean = 2), rnorm(n/2, mean = -2))
y <- c(rnorm(n/2, mean = 2), rnorm(n/2, mean = -2))

dat <- data.frame(type,x,y)
plot(y ~ x, data = dat)

write.csv(dat, file = 'ToyData.csv',row.names=FALSE)