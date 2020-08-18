data(mtcars)

library(ggplot2)

mpg_value <- 27

# qqplot to check distribution

qqnorm(mtcars$mpg)
qqline(mtcars$mpg)

qqnorm(mtcars$hp)
qqline(mtcars$hp)

# (1) Model 1

model1 <- lm(hp ~ mpg, data=mtcars)
pred1 <- predict(model1, newdata = data.frame(mpg = mpg_value))

# (2) model 2

mtcars$mpg2 <- ifelse(mtcars$mpg >= 20, mtcars$mpg-20, 0)

model2 <- lm(hp ~ mpg + mpg2, data=mtcars)

pred2 <- predict(model2, newdata = data.frame(mpg = mpg_value, mpg2=ifelse(mpg_value >= 20, mpg_value-20, 0)))

x <- 10:34
predVec1 <- predict(model1, newdata = data.frame(mpg=x))
x1 <- ifelse(x >= 20, x-20, 0)
predVec2 <- predict(model2, newdata = data.frame(mpg=x, mpg2=x1))
df <- data.frame(mpg=x1, hp=predVec2)

# base plot
plot(mtcars$mpg, mtcars$hp, pch=16, col='green')
abline(lm(hp~mpg, mtcars), col="blue")
lines(x, predVec2, col="purple", type="l")
points(mpg_value, pred1, col="red", pch=4, cex=1.2)
points(mpg_value, pred2, col="darkred", pch=4, cex=1.2)

# ggplot
ggplot(data=mtcars, aes(x=mpg, y=hp)) + geom_point(color='purple') + 
  stat_smooth(method=lm) + geom_line(mtcars$mpg, mtcars$hp)
                                     
# (3) Multiple regression lines
library(reshape2)
mtcars2 <- melt(mtcars, id.var='mpg')
ggplot(mtcars2) +
  geom_jitter(aes(value,mpg, colour=variable)) + geom_smooth(aes(value,mpg, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Percentage cover (%)", y = "Number of individuals (N)")
