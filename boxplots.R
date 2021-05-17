#values<-InsectSprays$count
#spraytype<-unique(InsectSprays$spray)
#datas<-split(values,spraytype)
#boxplot(datas)
#boxplot(InsectSprays$count~InsectSprays$spray)

par(mfrow=c(3,1))
library(dplyr)
data(nym.2002, package="UsingR")
males<-filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females<-filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
#boxplot(females, males)
hist(females,xlim=c(range(nym.2002$time)))
hist(males,xlim=c(range(nym.2002$time)))


