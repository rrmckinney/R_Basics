set.seed(1)

library(rafalib)

dat<-read.csv("mice_pheno.csv")
controlpopulation<-filter(dat,Sex=="F"&Diet=="chow")%>%select(Bodyweight)%>%unlist

ttestgenerator<-function(n){
  cases<-sample(controlpopulation, n)
  controls<-sample(controlpopulation,n)
  tstat<-(mean(cases)-mean(controls))/sqrt(var(cases)/n+var(controls)/n)
  return(tstat)
}

ttests<-replicate(1000, ttestgenerator(3))
hist(ttests)

qqnorm(ttests)
abline(0,1)

ps<-(seq(0,999)+0.5)/1000
qqplot(qt(ps, df=2*3-2),ttests, xlim=c(-6,6), ylim=c(-6,6))
abline(0,1)

controls<-rnorm(5000, mean=24, sd=3.5)

ttestgenerator<-function(n, mean=24, sd=3.5){
  cases<-rnorm(n,mean,sd)
  controls<-rnorm(n,mean,sd)
  tstat<-(mean(cases)-mean(controls))/sqrt(var(cases)/n+var(controls)/n)
  return(tstat)
}

ttests<-replicate(1000, ttestgenerator(3))
hist(ttests)
qqnorm(ttests)
abline(0,1)

ps<-(seq(0,999)+0.5)/1000
qqplot(qt(ps, df=2*3-2),ttests, xlim=c(-6,6), ylim=c(-6,6))
abline(0,1)
