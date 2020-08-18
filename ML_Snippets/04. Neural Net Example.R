# About layers
# Usually, if at all necessary, one hidden layer is enough for a vast numbers of applications. 
# As far as the number of neurons is concerned, 
# it should be between the input layer size and the output layer size, usually 2/3 of the input size. 
# At least in my brief experience testing again and again is the best solution since there is no guarantee that any of these rules will fit your model best.

# (1) nnet with iris

data(iris)
library(caret)
inTrain <- createDataPartition(iris$Species, p=0.75, list=F)
train <- iris[inTrain, ]
test <- iris[-inTrain, ]

library(nnet)
model <- nnet(Species~., data= train, size=5)

library(NeuralNetTools)
plotnet(model)

test_train <- predict(model, data=train, type="class")
test_test <- predict(model, newdata=test, type="class")

confusionMatrix(test_train, train$Species)$overall[1]
confusionMatrix(test_test, test$Species)$overall[1]

# (2) Neural Net with Spam dataset

library(kernlab)
data(spam)

# Neural net works best when normalised. But not sure if it applys to nnet.
# It applies to neuralnet() which only works for numeric values.

normalise <- function(x) {
  if (class(x) == "numeric"){
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)
  }
}

spam_norm <- as.data.frame(lapply(spam, normalise))

inTrain <- createDataPartition(spam_norm$type, p=0.75, list=F)
training_norm <- spam_norm[inTrain, ]
testing_norm <- spam_norm[-inTrain, ]

# Modeling and visualisation

model_nnet <- train(type ~., data=training_norm, method="nnet")

plot(model_nnet)

library(NeuralNetTools)
plotnet(model_nnet, alpha=0.6)

# predicting Outcome
predTrain <- predict(model_nnet, data=training_norm)
confusionMatrix(predTrain, training_norm$type)

predTest <- predict(model_nnet, newdata=testing_norm)
confusionMatrix(predTest, testing_norm$type)

# caret doesn't work when trying to specify the number of hidden layers
# Using nnet to specify hidden layers
# with incresed number of hidden layer, training accuracy improves, hoewever testing accuracy stays the same

model_nnet2 <- nnet(type ~., data=training_norm, size=10)

plot(model_nnet2) # this does not work with nnet. It works with caret & method="nnet"
plotnet(model_nnet2, alpha=0.6)

predTrain <- predict(model_nnet2, data=training_norm, type="class") # with nnet, type="class" is required
confusionMatrix(predTrain, training_norm$type)

predTest <- predict(model_nnet2, newdata=testing_norm, type="class")
confusionMatrix(predTest, testing_norm$type)

# Using  neuralnet. It does not take ~. so it doesn't work...
library(neuralnet)
model_nuralnet <- neuralnet(type ~., data=training_norm, hidden=4)
pred <- compute(model_nuralnet, training)


# (3) neuralnet with iris. formula y~. is not accepted
#   make sure to use linear.output=False for classification. linear.output=True is default and for numeric prediction.
#   neural net seems to only work on numeric type. Probably normalisation is required...
data(iris)
iris$Species <- ifelse(iris$Species=="setosa", 1,
                       ifelse(iris$Species=="versicolor" ,2,
                               ifelse(iris$Species=="virginica",3, '')))
iris$Species <- as.numeric(iris$Species)
library(caret)
inTrain <- createDataPartition(iris$Species, p=0.75, list=F)
train <- iris[inTrain, ]
test <- iris[-inTrain, ]

library(neuralnet)
model <-neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train,  
                  hidden=3,linear.output=F)

plot(model)

n<- neuralnet(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,err.fct="sse",hidden=c(3),linear.output=FALSE)

# (5) nnet with Factor variables, predicting binary outcome
#     Predict Health outcome (binary) with nnet

library(ISLR); library(caret); library(nnet); library(NeuralNetTools)

data(Wage)

df <- subset(Wage, select = -c(health, sex, region))
dmy <- dummyVars("~.", data=df)
trsf <- data.frame(predict(dmy, newdata=df))
wage <- cbind(trsf, health=Wage$health)

normalise <- function(x) {
  if (class(x) == "numeric"){
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)
  }
}

wage_norm<- as.data.frame(lapply(wage, normalise))

inTrain <- createDataPartition(wage_norm$health, p=0.75, list=F)
train <- wage_norm[inTrain, ]
test <- wage_norm[-inTrain, ]

model <- nnet(health ~., data=train, size=15)
plotnet(model)

test_train <- predict(model, data=train, type="class")
test_test <- predict(model, newdata=test, type="class")

confusionMatrix(test_train, train$health)
confusionMatrix(test_test, test$health)

model <- nnet(health~logwage+wage+age, data=train, size=3)
plotnet(model)

# (6) nnet with Factor variable, predicting numerical output, wage
library(ISLR)
data(Wage)


# (5) neuralnet with concrete dataset from Machine Learning with R textbook



###############################################################################
##
###############################################################################

# This does not work because nnet needs numeric variables. Therefore, we have to translate text information to numerical information.

library(ISLR); library(caret); library(nnet); library(NeuralNetTools)

data(Wage)

inTrain <- createDataPartition(Wage$health, p=0.75, list=F)
train <- Wage[inTrain, ]
test <- Wage[-inTrain, ]

model <- nnet(health ~., data=train, size=10, skip=TRUE)
plotnet(model)

test_train <- predict(model, data=train, type="class")
test_test <- predict(model, newdata=test, type="class")

confusionMatrix(test_train, train$health)
confusionMatrix(test_test, test$health)

model <- nnet(health~logwage+wage+age, data=train, size=3)
plotnet(model)

