setwd("C:\\Users\\s174300\\Documents\\00 DTU\\5. semester\\02445 Project in statistics\\02445-Projekt")
load("armdata.RData")

#setting up the dataframe
person <- c()
repetition <- c()
step <- c()
x.loc <- c()
y.loc <- c()
z.loc <- c()
for(i in 1:10){
  person <- append(person, rep(i, 1000))
  for(j in 1:10){
    repetition <- append(repetition, rep(j, 100))
    step <- append(step, seq(1, 100, 1))
    x.loc <- append(x.loc, armdata[[11]][[i]][[j]][,1])
    y.loc <- append(y.loc, armdata[[11]][[i]][[j]][,2])
    z.loc <- append(z.loc, armdata[[11]][[i]][[j]][,3])
  }
}
df <- data.frame(person, repetition, step, x.loc, y.loc, z.loc)

apply(df,2,function(x) sum(is.na(x))) # er der missin values?
# Vi ser person  mangler data punkter på repetition 1 step 1 og 2, derfor ændres disse til step 3
df[8001:8002,]$x.loc = df[8003,]$x.loc
df[8001:8002,]$y.loc = df[8003,]$y.loc
df[8001:8002,]$z.loc = df[8003,]$z.loc

save(df, file="df.RData")
