setwd("/home/nker/OneDrive/Bsc/02445 projekt i statistisk evaluering af kunstig inteligens/projects/2/")
load("fosfor_data.Rdata")
summary(Phosphorous)
apply(Phosphorous,2,function(x) sum(is.na(x))) # er der missin values?

# fjerner NA fra data
phos.data = Phosphorous[ which(!is.na(Phosphorous$yield)), ]

phos.data$location = as.factor(phos.data$location)

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
plot(phos.data$olsenP, phos.data$yield, xlab = "mg/100g phosphate measurement with Olsen P", ylab = "hkg barley yield", main = "Barley yield to phosphate measurement with Olsen P", xlim=c(0, max(temp.data$olsenP)))
olsenP.x = seq(from = 0, to = max(phos.data$olsenP), length.out = 100)
olsenP.y = olsenP.args[1] * olsenP.x / (olsenP.args[2] + olsenP.x)
lines(olsenP.x, olsenP.y, col="red")

#hist(phos.data$olsenP)



summary(aov(yield ~ location + DGT + olsenP, data = phos.data))




## Gennemsnit
phos.data.n = aggregate(phos.data[,2:4], list(phos.data$location), mean)

phos.data.n$location = phos.data.n$Group.1

phos.data.n$location = as.factor(phos.data.n$location)

phos.model.DGT.n <- nls(yield ~ alpha * DGT/(beta + DGT), data=phos.data.n, start=list(alpha=90, beta=1))
phos.model.olsenP.n <- nls(yield ~ alpha * olsenP/(beta + olsenP), data=phos.data.n, start=list(alpha=90, beta=1))

DGT.args = phos.model.DGT.n$m$getPars()
olsenP.args = phos.model.olsenP.n$m$getPars()

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



# Residual plots

plot(fitted(phos.model.DGT), resid(phos.model.DGT), xlab = "Fitted DGT measurement", ylab = "Residuals", main = "Residuals vs Fitted DGT measurements") # [which(phos.data$location != "011")]
plot(fitted(phos.model.olsenP), resid(phos.model.olsenP), xlab = "Fitted Olsen P measurement", ylab = "Residuals", main = "Residuals vs Fitted Olsen P measurements")


for(i in c("001","002","003","004","006","007","008","010","011")){
  temp.data = phos.data[which(phos.data$location != i),]
  temp.model.DGT <- nls(yield ~ alpha * DGT/(beta + DGT), data=temp.data, start=list(alpha=90, beta=1))
  plot(temp.data$DGT, temp.data$yield, xlab = "μg/L phosphate measurement with DGT", ylab = "hkg barley yield", main = "Barley yield to phosphate measurement with DGT",xlim=c(0, max(temp.data$DGT)))
  DGT.x = seq(from = 0, to = max(temp.data$DGT), length.out = 100)
  y = predict(temp.model.DGT, list(DGT = DGT.x))
  lines(DGT.x, y, col="red")
  print(paste("Location:",i))
  print(summary(temp.model.DGT))
}
