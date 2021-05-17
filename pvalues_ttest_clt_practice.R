library(downloader)
library(rafalib)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

n=100
set.seed(1)
dieRolls <- replicate(10000, mean(sample(1:6, n, replace=TRUE)==6))
p<-1/6
var<-p*(1-p)/n
z<-(dieRolls - p) / sqrt(p*(1-p)/n)
mypar(2,2)
hist(z)
qqnorm(z)
qqline(z)
mean(abs(z)>2)

# model answer
set.seed(1)
n <- 30
sides <- 2
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)

library(dplyr)
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

mean(X)
sd(X) # this one, since we have a sample.

# Use the CLT to approximate the probability that our estimate X¯ is off 
# by more than 2 grams from ??X.

se<-sd(X)/sqrt(12)
z<-2/se
pnorm(z,lower.tail=FALSE)*2

# sd of mean(X) - mean(Y)
se<-sqrt(var(X)/12 + var(Y)/12)
se

(mean(Y)-mean(X))/se

1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)

pnorm(2.055174,lower.tail=FALSE)*2

t.test(X,Y)
