dim(dat)

par(mfrow=c(3,3))

for (i in 1:9){
  x<-rt(1000,i)
  qqnorm(x)
  qqline(x)
  hist(dat[,i], xlim=c(-20,20))
}
