data(mtcars)

######################################
# (1) PCA
######################################

prComp <- princomp(mtcars[, -4], cor=T)

summary(prComp)

loadings(prComp)

plot(prComp, type="lines")

biplot(prComp)

head(prComp$scores)

######################################
# (2) Linear Regression with PCA trial
######################################

df <- as.data.frame(prComp$scores)
df$hp <- mtcars$hp

plot(df$Comp.1, df$Comp.2)

summary(df)
library(caret)

inTrain <- createDataPartition(df$hp, p=0.7, list=F)
training <- df[inTrain, ]
testing <- df[-inTrain, ]

modelReg <- lm(hp~Comp.1+Comp.2, training)
modelReg

predModelReg <- predict(modelReg, newdata=training)
plot(predModelReg, training$hp)
abline(a=0, b=1)

predModelReg_Test <- predict(modelReg, newdata=testing)
plot(predModelReg_Test, testing$hp)
abline(a=0, b=1)
