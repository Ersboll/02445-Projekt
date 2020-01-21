setwd("/home/nker/OneDrive/Bsc/02445 projekt i statistisk evaluering af kunstig inteligens/02445-Projekt/")

library(neuralnet)

load("df.RData")

person <- c()
repetition <- c()
loc <- matrix(0,nrow=100, ncol=300)
loc.arg <- c()


for(i in 1:10){
  person <- append(person, rep(i, 10))
  for(j in 1:10){
    repetition <- append(repetition, j)
    temp = which(df$repetition == j & df$person == i)
    loc[i*10 -10 + j,] = c(df[temp,]$x.loc, df[temp,]$y.loc, df[temp,]$z.loc)
  }
}

arm.data <- data.frame(person, repetition, loc) # loc = 1..100 er x, loc = 101..200 er y, loc = 201..300 er z
arm.data$person <- as.factor(arm.data$person)
arm.test = which(arm.data$repetition == 1)
arm.train = setdiff(1:100,arm.test)

f = paste("(person == 1) + (person == 2) + (person == 3) + (person == 4)  + (person == 5)  + (person == 6)  + (person == 7)  + (person == 8)  + (person == 9)  + (person == 10) ~", paste(paste("X", 1:300, " + ", sep=""),collapse = " "))
f = substr(f, 1, nchar(f)-3)
as.formula(f)

# normalizing
arm.data[,3:302] = as.data.frame(scale(arm.data[,3:302], center = apply(arm.data[,3:302], 2, min), scale = apply(arm.data[,3:302], 2, max) - apply(arm.data[,3:302], 2, min)))

nn <- neuralnet(f, data=arm.data[arm.train,], hidden = c(50,30,15), stepmax = 1e+06, threshold = 0.00001, linear.output=FALSE)
pred = predict(nn, arm.data[arm.train,])
mean(max.col(pred) == arm.data[arm.train,]$person)

pred = predict(nn, arm.data[arm.test,])
mean(max.col(pred) == arm.data[arm.test,]$person)

for(i in 1:length(pred[,1])){
  print(paste(arm.data[arm.test,]$person[i], which(max(pred[i,]) == pred[i,])))
}

mean(max.col(pred) == c(1,2,3,4,5,6,7,8,9,10))

out <- rep(0,5)
pr.out <- matrix(data=0, nrow = 20, ncol = 5)

set.seed(2002)

# 5 fold crossvalidation
for(i in 1:5){
  arm.test <- which(arm.data$repetition == i*2 | arm.data$repetition == i*2-1)
  arm.train <- setdiff(1:100,arm.test)
  
  nn_cv <- neuralnet(f, 
                     data=arm.data[arm.train,], 
                     hidden = c(30),
                     stepmax = 1e+05,
                     threshold = 0.001, 
                     linear.output=FALSE,
                     act.fct = "logistic")
  # Compute predictions
  pr.nn <- predict(nn_cv, arm.data[arm.test,])
  # Accuracy on test set
  #out[i] = mean(max.col(pr.nn) == arm.data[arm.test,]$person)
  pr.out[,i] = max.col(pr.nn) == arm.data[arm.test,]$person
  print(i)
}

mean(pr.out)
