setwd("C:\\Users\\s174300\\Documents\\00 DTU\\5. semester\\02445 Project in statistics\\02445-Projekt")
load('armdata.RData')

#CREATE A DATAFRAME OF MEANS
xdata <- c()
ydata <- c()
zdata <- c()
for(i in 1:16){ #experiment
  experiment.xmeans <- c()
  experiment.ymeans <- c()
  experiment.zmeans <- c()
  for(j in 1:10){ #person
    xallreps <- c()
    yallreps <- c()
    zallreps <- c()
    for(k in 1:10){ #repetition
      xs <- armdata[[i]][[j]][[k]][,1][ which(!is.na(armdata[[i]][[j]][[k]][,1]))]
      ys <- armdata[[i]][[j]][[k]][,2][ which(!is.na(armdata[[i]][[j]][[k]][,2]))]
      zs <- armdata[[i]][[j]][[k]][,3][ which(!is.na(armdata[[i]][[j]][[k]][,3]))]
      xallreps <- append(xallreps, xs)
      yallreps <- append(yallreps, ys)
      zallreps <- append(zallreps, zs)
    }
    xmean <- mean(xallreps)
    experiment.xmeans <- append(experiment.xmeans, xmean)
    ymean <- mean(yallreps)
    experiment.ymeans <- append(experiment.ymeans, ymean)
    zmean <- mean(zallreps)
    experiment.zmeans <- append(experiment.zmeans, zmean)
  }
  if(i == 1){
    xdata <- experiment.xmeans
    ydata <- experiment.ymeans
    zdata <- experiment.zmeans
  }
  else{
    xdata <- data.frame(xdata, experiment.xmeans)
    ydata <- data.frame(ydata, experiment.ymeans)
    zdata <- data.frame(zdata, experiment.zmeans)
  }
}

#PLOT MEANS AS BOXPLOTS
boxplot(xdata, ylab='Mean x values', main='Boxplot for each experiment, x values')
boxplot(ydata, ylab='Mean y values', main='Boxplot for each experiment, y values')
boxplot(zdata, ylab='Mean z values', main='Boxplot for each experiment, z values')

xdatalist <- as.vector(t(xdata))
ydatalist <- as.vector(t(ydata))
zdatalist <- as.vector(t(zdata))

#HISTOGRAMS OF MEANS
hist(xdatalist, xlab='Mean x values', freq = FALSE, main='Distribution of mean x values')
hist(ydatalist, xlab='Mean y values', freq = FALSE, main='Distribution of mean y values')
hist(zdatalist, xlab='Mean z values', freq = FALSE, main='Distribution of mean z values')

#MAUCHLY'S TEST FOR SPHERICITY
xdatamatrix <- data.matrix(xdata) 
mlm <- lm(xdatamatrix~1)
mauchly.test(mlm, x = ~ 1)
ydatamatrix <- data.matrix(ydata) 
mlm <- lm(ydatamatrix~1)
mauchly.test(mlm, x = ~ 1)
ydatamatrix <- data.matrix(zdata) 
mlm <- lm(zdatamatrix~1)
mauchly.test(mlm, x = ~ 1)