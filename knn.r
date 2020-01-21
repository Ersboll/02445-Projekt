library(class)
library(gmodels)
setwd("C:\\Users\\s174300\\Documents\\00 DTU\\5. semester\\02445 Project in statistics\\02445-Projekt")
load("armdata.RData")

#NORMALIZATION (feature scaling) OF X Y and Z COORDINATES
nor <- function(x) { (x-min(x))/(max(x)-min(x)) }

#DATA
data <- c()
for(i in 1:10){
  for(j in 1:10){
    ls <- c()
    ls <- append(ls, i) #class
    if(i == 9){
      if(j == 1){
        ls <- append(ls, armdata[[11]][[i]][[j]][,1][3])
        ls <- append(ls, armdata[[11]][[i]][[j]][,1][3])
      }
    }
    ls <- append(ls, armdata[[11]][[i]][[j]][,1][which(!is.na(armdata[[11]][[i]][[j]][,1]))])
    if(i == 9){
      if(j == 1){
        ls <- append(ls, armdata[[11]][[i]][[j]][,2][3])
        ls <- append(ls, armdata[[11]][[i]][[j]][,2][3])
      }
    }
    ls <- append(ls, armdata[[11]][[i]][[j]][,2][which(!is.na(armdata[[11]][[i]][[j]][,2]))])
    if(i == 9){
      if(j == 1){
        ls <- append(ls, armdata[[11]][[i]][[j]][,3][3])
        ls <- append(ls, armdata[[11]][[i]][[j]][,3][3])
      }
    }
    ls <- append(ls, armdata[[11]][[i]][[j]][,3][which(!is.na(armdata[[11]][[i]][[j]][,3]))])
    data <- rbind(data, ls)
  }
}


accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}


#CROSSVALIDATION
for(k in 1:5){
  for(i in 2:301){
    data[,i] <- nor(data[,i]) 
  } 
  
  test <- data[seq(k,100,5),seq(2,301)]
  train <- data[setdiff(seq(1,100), seq(k,100,5)),seq(2,301)]
  test_cat <- data[seq(k,100,5), 1]
  train_cat <- data[setdiff(seq(1,100), seq(k,100,5)), 1]
  
  pr <- knn(train, test, cl=train_cat, k=10)
  tab <- table(pr, test_cat)
  print(k)
  print(tab)
  print(accuracy(tab))
}
