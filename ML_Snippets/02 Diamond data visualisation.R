library(UsingR)
data(diamonds)
library(ggplot2)

sample <- sample(1:nrow(diamonds), 500)

df <- diamonds[sample,]

ggplot(aes(x=carat, y=price, color=cut), data=df) + geom_point() + geom_smooth(method=lm) + facet_wrap("cut")
