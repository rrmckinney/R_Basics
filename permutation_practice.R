babies<-read.table("babies.txt", header=TRUE)

bwt.nonsmoke<-filter(babies,smoke==0)%>%select(bwt)%>%unlist
bwt.smoke<-filter(babies,smoke==1)%>%select(bwt)%>%unlist

N=10
set.seed(1)
nonsmokers<-sample(bwt.nonsmoke,N)
smokers<-sample(bwt.smoke,N)
obs<-median(smokers)-median(nonsmokers)
null<- replicate(1000, {
  dat<-c(nonsmokers,smokers)
  shuffle<- sample(dat)
  smokersstar<- shuffle[1:N]
  nonsmokersstar<- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
null
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null) >= abs(obs)) ) / ( length(null) )

