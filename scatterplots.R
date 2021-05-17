install.packages("UsingR")
library(UsingR)
library(dplyr)
data(nym.2002, package="UsingR")
dat<-nym.2002
names(dat)
fem<-filter(nym.2002, gender=="Female")
men<-filter(nym.2002, gender=="Male")
cor(men$time, men$age)
cor(fem$time, fem$age)
plot(fem$age,fem$time)

time<-sort(nym.2002$time)
ans<-time[length(time)]/median(time)
ans

par(mfrow=c(1,2))
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

plot(log2(time/median(time)), ylime=c(-2,2))
abline(h=-1:1)
