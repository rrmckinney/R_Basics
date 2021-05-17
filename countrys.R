library(gapminder)
data(gapminder)
head(gapminder)

gapminder<-filter(gapminder, year=="1952")
lifeExp<-gapminder$lifeExp

mean(lifeExp<=60)-mean(lifeExp<=40)

prop = function(q) {
  mean(x <= q)
}
qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
plot(qs, props)

plot(ecdf(x))
