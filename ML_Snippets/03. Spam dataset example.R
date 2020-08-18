
library(kernlab)
data(spam)

# (1) Plot density against frequency of yours for both spam and nonspam in one graph.
#     Assume when the frequency of your is more than 0.5, the email is spam. 
#     Then, calculate accuracy according to the prediction

plot(density(spam$your[spam$type=="nonspam"]), col="blue", main="", xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]), col="red")
abline(v=0.5, col="black")

prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")

tab <- as.matrix(table(prediction, spam$type)/length(spam$type))
tab

accuracy <- tab[1] + tab[4]
accuracy

# (2) Use caret package, split df to training and testing.
#     Use logistic regression to create a model on training set and check accuracy on testing
#     create a model with random forest

library(caret)
inTrain <- createDataPartition(spam$type, p=0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# Create a model by using train() 

model <- train(type ~., training, method="glm") # change to method = "glm" for logistic regression
stopCluster(workers)

# Use on training set
predTrain <- predict(model, data=training)

# Calculate Accuracy
tabTrain <- as.matrix(table(predTrain, training$type))
tabTrain
tabTrain[1]/nrow(training) + tabTrain[4]/nrow(training)

# Alternatively use confusion matrix
confusionMatrix(predTrain, training$type)

# Test the model on testing set
predTest <- predict(model, newdata=testing)
# Calculate Accuracy
tabTest <- as.matrix(table(predTest, testing$type))
tabTest
tabTest[1]/nrow(testing) + tabTest[4]/nrow(testing)
# Alternatively use confusion matrix
confusionMatrix(predTest, testing$type)

# (3) Neural net

# Neural net works best when normalised

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


# (10) Exploring parallel capability of caret package
#
# caret will automatically paralellize if a backend is registered 
# By using doParallel, I can reduce user and system time. But not elapse time, which mean something is going wrong...
# http://michaeljkoontz.weebly.com/uploads/1/9/9/4/19940979/parallel.pdf


library(doParallel)
workers=makeCluster(detectCores())
registerDoParallel(workers) # or just registerDoParallel(cores=4) without defining workers
system.time(train(type ~., training, method="rf"))
stopCluster(workers)


# further investigation on parallel processing 
# doParallel makes it slower

data(iris)
library(doParallel)
workers=makeCluster(4)
registerDoParallel(workers)
set.seed(1000) # setting seed doesn't work
system.time(train(Species ~., data=iris, method="rf"))
