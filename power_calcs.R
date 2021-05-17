library(dplyr)
dat<-read.csv("mice_pheno.csv")
controlpopulation<-filter(dat,Sex=="F"&Diet=="chow")%>%select(Bodyweight)%>%unlist
hfpopulation<-filter(dat, Sex=="F"&Diet=="hf")%>%select(Bodyweight)%>%unlist

mu_hf<-mean(hfpopulation)
mu_control<-mean(controlpopulation)
print(mu_hf-mu_control)
print(mu_hf-mu_control)/mu_control*100

set.seed(1)
N<-12
hf<-sample(hfpopulation, N)
control<-sample(controlpopulation, N)
t.test(hf,control)$p.value

alpha<-0.05

B<-2000
reject<-function(N,alpha=0.05){
  hf<-sample(hfpopulation,N)
  control<-sample(controlpopulation,N)
  pval<-t.test(hf,control)$p.value
  pval<alpha
}
rejections<-replicate(B,reject(N))
mean(rejections)

Ns<-seq(5,50,5)

power<-sapply(Ns,function(N){
  rejections<-replicate(B,reject(N))
  mean(rejections)
})
plot(Ns, power, type="b")
