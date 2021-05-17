babies<-read.table("babies.txt", header=TRUE)

bwt.nonsmoke<-filter(babies,smoke==0)%>%select(bwt)%>%unlist
bwt.smoke<-filter(babies,smoke==1)%>%select(bwt)%>%unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
N<-25
dat.ns<-sample(bwt.nonsmoke,N)
dat.s<-sample(bwt.smoke,N)


tval<-t.test(dat.ns,dat.s)
tval<-1.6593
pval1<-1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval2<-2*pnorm(-abs(tval))
pval1
pval2
         