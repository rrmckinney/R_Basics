library(dplyr)
dat<-read.csv("femaleMiceWeights.csv")
controls<-filter(dat,Diet=="chow")%>%select(Bodyweight)%>%unlist
treatment<-filter(dat,Diet=="hf")%>%select(Bodyweight)%>%unlist
mean(controls)
sd(controls)
sqrt(var(controls))
obs<-mean(controls)

x<-read.csv("femaleControlsPopulation.csv")
x<-unlist(x)

set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages5[i] <- mean(X)
}
hist(averages5) ##take a look
mean( abs( averages5 - mean(x) ) > 1)

