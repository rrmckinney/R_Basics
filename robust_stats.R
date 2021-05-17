data("ChickWeight")
head(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

chick<-reshape(ChickWeight, idvar=c("Chick", "Diet"),timevar="Time", direction="wide")
head(chick)
chick<-na.omit(chick)
ans<-mad(c(chick$weight.4, 3000))/mad(chick$weight.4)
ans
one<-cor(chick$weight.4, chick$weight.21)
chick4<-c(chick$weight.4,3000)
chick21<-c(chick$weight.21,3000)
two<-cor(chick4, chick21)
two/one

x<-filter(chick, Diet=="1")
x<-x$weight.4
y<-filter(chick, Diet=="4")
y<-y$weight.4    

t.test(x,y)
wilcox.test(x,y)

library(rafalib)
par(mfrow=c(1,3))
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

un<-t.test(x,y+10)$statistic
deux<-t.test(x,y+100)$statistic

un-deux

wilcox.test(c(1,2,3),c(4,5,6))$p.value
wilcox.test(c(400,500,600), c(1,2,3))$p.value
