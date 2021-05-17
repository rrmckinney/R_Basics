set.seed(1)
n<-30
p<-0.5
z<-replicate(10000,(mean(sample(1:6, n, replace=TRUE)==6) - p) / sqrt(p*(1-p)/n))

mean(abs(z)>=2)
qqnorm(z)
qqline(z)


