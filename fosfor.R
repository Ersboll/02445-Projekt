setwd("C:\\Users\\s174300\\Documents\\00 DTU\\5. semester\\02445 Project in statistics\\02445-Projekt")
load('fosfor_data.RData')
phos.data = Phosphorous[ which(!is.na(Phosphorous$yield)),]

#summary and data visualization
summary(phos.data)
hist(phos.data$yield, xlab='yield', main='Distribution of yield', freq=FALSE)
hist(phos.data$DGT, xlab='DGT', main='Distribution of DGT', freq=FALSE)
hist(phos.data$olsenP, xlab='olsenP', main='Distribution of olsenP', freq=FALSE)
hist(log(phos.data$olsenP), xlab='log(olsenP)', main='Distribution of log(olsenP)', freq=FALSE)
plot(phos.data$DGT, phos.data$yield, xlab='DGT', ylab='yield', main='DGT and yield')
plot(phos.data$olsenP, phos.data$yield, xlab='olsenP', ylab='yield', main='olsenP and yield')


#get alpha and beta for each model
phos.model <- nls(yield ~ alfa * DGT/(beta + DGT) , data = phos.data, start = list(alfa = 90 , beta = 1))
phos.model
phos.model <- nls(yield ~ alfa * olsenP/(beta + olsenP) , data = phos.data, start = list(alfa = 90 , beta = 1))
phos.model 


#simulate yield from each model
DGT.pred <- 77.342 * phos.data$DGT/(4.896 + phos.data$DGT)
olsenP.pred <- 81.1747 * phos.data$olsenP/(0.7109 + phos.data$olsenP) 
hist(DGT.pred, freq=FALSE, xlab='yield', main='Distribution of yield using DGT model')
hist(olsenP.pred, freq=FALSE, xlab='yield', main='Distribution of yield using olsenP model') 
