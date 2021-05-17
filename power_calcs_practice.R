babies<-read.table("babies.txt", header=TRUE)

bwt.nonsmoke<-filter(babies,smoke==0)%>%select(bwt)%>%unlist
bwt.smoke<-filter(babies,smoke==1)%>%select(bwt)%>%unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
N<-90

B<-10000
reject<-function(N,alpha=0.01){
  dat.ns<-sample(bwt.nonsmoke,N)
  dat.s<-sample(bwt.smoke,N)
  pval<-t.test(dat.ns,dat.s)$p.value
  pval<alpha
  }
rejections<-replicate(B,reject(N))
mean(rejections)
