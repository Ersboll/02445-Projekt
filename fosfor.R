setwd("/home/nker/OneDrive/Bsc/02445 projekt i statistisk evaluering af kunstig inteligens/projects/2/")
load("fosfor_data.Rdata")
summary(Phosphorous)
apply(Phosphorous,2,function(x) sum(is.na(x))) # er der missin values?

# Imputation
#phos.data = Phosphorous[ which(!is.na(Phosphorous$yield)), ]
phos.data = Phosphorous
phos.data[c(34,36),2] = rep(mean(phos.data$yield[which(phos.data$location == "011")], na.rm = T), 2)
phos.data$location = as.factor(phos.data$location)

plot(phos.data$DGT, phos.data$yield, xlab = "μg/L phosphate measurement with DGT", ylab = "hkg barley yield", main = "Barley yield to phosphate measurement with DGT", xlim=c(0, max(phos.data$DGT)))
plot(phos.data$olsenP, phos.data$yield, xlab = "mg/100g phosphate measurement with Olsen P", ylab = "hkg barley yield", main = "Barley yield to phosphate measurement with Olsen P", xlim=c(0, max(phos.data$olsenP)))

phos.model.DGT <- nls(yield ~ alpha * DGT/(beta + DGT), data=phos.data, start=list(alpha=90, beta=1))
phos.model.olsenP <- nls(yield ~ alpha * olsenP/(beta + olsenP), data=phos.data, start=list(alpha=90, beta=1))

print(summary(phos.model.DGT))
print(summary(phos.model.olsenP))

DGT.args = phos.model.DGT$m$getPars()
olsenP.args = phos.model.olsenP$m$getPars()


### DGT
plot(phos.data$DGT, phos.data$yield, xlab = "μg/L phosphate measurement with DGT", ylab = "hkg barley yield", main = "Barley yield to phosphate measurement with DGT", xlim=c(0, max(phos.data$DGT))) # col=c("red","green","blue","yellow","orange","magenta","pink","purple","black")[phos.data$location]
DGT.x = seq(from = 0, to = max(phos.data$DGT), length.out = 100)
DGT.y = DGT.args[1] * DGT.x / (DGT.args[2] + DGT.x)
lines(DGT.x, DGT.y, col="red")

#hist(phos.data$DGT)


### OLSEN.P
plot(phos.data$olsenP, phos.data$yield, xlab = "mg/100g phosphate measurement with Olsen P", ylab = "hkg barley yield", main = "Barley yield to phosphate measurement with Olsen P", xlim=c(0, max(phos.data$olsenP)))
olsenP.x = seq(from = 0, to = max(phos.data$olsenP), length.out = 100)
olsenP.y = olsenP.args[1] * olsenP.x / (olsenP.args[2] + olsenP.x)
lines(olsenP.x, olsenP.y, col="red")

#hist(phos.data$olsenP)




# Residual plots

plot(fitted(phos.model.DGT), resid(phos.model.DGT), xlab = "Fitted yield with DGT measurement", ylab = "Residuals", main = "Residuals vs Fitted yield with DGT measurements") # [which(phos.data$location != "011")]
plot(fitted(phos.model.olsenP), resid(phos.model.olsenP), xlab = "Fitted yield with Olsen P measurement", ylab = "Residuals", main = "Residuals vs Fitted yield with Olsen P measurements")

plot(fitted(phos.model.DGT)[which(phos.data$location != "011")], resid(phos.model.DGT)[which(phos.data$location != "011")], xlab = "Fitted yield with DGT measurement", ylab = "Residuals", main = "Residuals vs Fitted yield with DGT measurements")

# DGT
plot(phos.data.n$DGT, phos.data.n$yield, xlab = "μg/L phosphate measurement with DGT", ylab = "hkg barley yield", main = "Barley yield to phosphate measurement with DGT")
DGT.x = seq(from = 0, to = max(phos.data.n$DGT), length.out = 100)
DGT.y = DGT.args[1] * DGT.x / (DGT.args[2] + DGT.x)
lines(DGT.x, DGT.y, col="red")

### OLSEN.P
plot(phos.data.n$olsenP, phos.data.n$yield, xlab = "mg/100g phosphate measurement with Olsen P", ylab = "hkg barley yield", main = "Barley yield to phosphate measurement with Olsen P")
olsenP.x = seq(from = 0, to = max(phos.data.n$olsenP), length.out = 100)
olsenP.y = olsenP.args[1] * olsenP.x / (olsenP.args[2] + olsenP.x)
lines(olsenP.x, olsenP.y, col="red")

par(mfrow=c(3,3))

sigma = c()
beta = c()
beta.p = c()
for(i in c("001","002","003","004","006","007","008","010","011")){
  temp.data = phos.data[which(phos.data$location != i),]
  temp.model.DGT <- nls(yield ~ alpha * DGT/(beta + DGT), data=temp.data, start=list(alpha=90, beta=1))
  plot(temp.data$DGT, temp.data$yield, xlab = "μg/L phosphate DGT measurement", ylab = "hkg barley yield", main = paste("Removed field:",i),xlim=c(0, max(temp.data$DGT)))
  DGT.x = seq(from = 0, to = max(temp.data$DGT), length.out = 100)
  y = predict(temp.model.DGT, list(DGT = DGT.x))
  lines(DGT.x, y, col="red")
  #print(paste("Location:",i))
  sum = summary(temp.model.DGT)
  #print(sum)
  sigma <- append(sigma, sum$sigma)
  beta <- append(beta, sum$coefficients[2,1])
  beta.p <- append(beta.p, sum$coefficients[2,4])
}
print(sigma)
print(beta)
print(beta.p)

sigma = c()
beta = c()
beta.p = c()
for(i in c("001","002","003","004","006","007","008","010","011")){
  temp.data = phos.data[which(phos.data$location != i),]
  temp.model.olsenP <- nls(yield ~ alpha * olsenP/(beta + olsenP), data=temp.data, start=list(alpha=90, beta=1))
  plot(temp.data$olsenP, temp.data$yield, xlab = "μg/L phosphate olsenP measurement", ylab = "hkg barley yield", main = paste("Removed field:",i),xlim=c(0, max(temp.data$olsenP)))
  olsenP.x = seq(from = 0, to = max(temp.data$olsenP), length.out = 100)
  y = predict(temp.model.olsenP, list(olsenP = olsenP.x))
  lines(olsenP.x, y, col="red")
  print(paste("Location:",i))
  sum = summary(temp.model.olsenP)
  print(sum)
  sigma <- append(sigma, sum$sigma)
  beta <- append(beta, sum$coefficients[2,1])
  beta.p <- append(beta.p, sum$coefficients[2,4])
}
print(sigma)
print(beta)
print(beta.p)
 
