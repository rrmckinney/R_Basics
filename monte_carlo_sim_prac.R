set.seed(1)

x<-rnorm(5)
t.test(x)

Ns<-5
ttestgenerator<-function(5){
  x<-rnorm(5)
  #tstat<-sqrt(Ns)*mean(x)/sd(x)
  tstat<-t.test(x, var.equal=TRUE)$t
  return(tstat)
}

ttests<-replicate(1000, ttestgenerator(5))
qqnorm(ttests)

B<-100
quant<-qt(seq(1/(B+1), 1-1/(B+1), len=B), df=4)
qqplot(ttests,quant)
