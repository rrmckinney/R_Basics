babies<-read.table("babies.txt", header=TRUE)

bwt.nonsmoke<-filter(babies,smoke==0)%>%select(bwt)%>%unlist
bwt.smoke<-filter(babies,smoke==1)%>%select(bwt)%>%unlist

library(rafalib)
meandiff<-mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
N<-5
dat.ns<-sample(bwt.nonsmoke, N)
dat.s<-sample(bwt.smoke, N)
t.test(dat.ns,dat.s)

Q<-qt(1-0.01/2, df=2*N-2)

interval<-Q*sqrt(sd(dat.ns)^2/N+sd(dat.s)^2/N)
interval

meandiff
