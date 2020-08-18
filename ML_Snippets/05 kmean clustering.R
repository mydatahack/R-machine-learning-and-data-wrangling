library(ISLR); library(caret); library(nnet); library(NeuralNetTools); library(ggplot2)

data(Wage)

df <- subset(Wage, select = -c(sex, region))
dmy <- dummyVars("~.", data=df)
wage <- data.frame(predict(dmy, newdata=df))

normalise <- function(x) {
  if (class(x) == "numeric"){
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)
  }
}

wage_norm<- as.data.frame(lapply(wage, normalise))

inTrain <- createDataPartition(wage_norm$year, p=0.75, list=F)
train <- wage_norm[inTrain, ]
test <- wage_norm[-inTrain, ]

cluster <- kmeans(train, centers=3)
train$clusters <- as.factor(cluster$cluster)

table(train$clusters)
 
ggplot(data=train, aes(x=age, y=logwage, color=clusters)) + geom_point()

model_train <- train(clusters~., data=train, method="nnet")
plotnet(model_train)
test_train <- predict(model_train, data=train)
confusionMatrix(test_train, train$clusters)

test_test <- predict(model_train, newdata=test)

# This is pointless 
cluster <- kmeans(test, centers=3)
test$clusters <- as.factor(cluster$cluster)
confusionMatrix(test_test,test$cluster)

# hierarchical clustering
clusters <-hclust(dist(train))
plot(clusters)
#specifies cluster number
clusterCut <- cutree(clusters, 3)
