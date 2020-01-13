setwd("/home/nker/OneDrive/Bsc/02445 projekt i statistisk evaluering af kunstig inteligens/projects/1/")


library(nnet)

load("armdata.RData")

apply(armdata[[11]],2,function(x) sum(is.na(x))) # er der missin values?

arm.train<- sample(1:10,7)
arm.test <- setdiff(10,arm.train)
ideal <- class.ind(arm$person)

seedsANN = nnet(arm[seedstrain,1], ideal[seedstrain,], size=10, softmax=TRUE)
predict(seedsANN, seeds[seedstest,-8], type="class")
table(predict(seedsANN, seeds[seedstest,-8], type="class"),seeds[seedstest,]$X1)
