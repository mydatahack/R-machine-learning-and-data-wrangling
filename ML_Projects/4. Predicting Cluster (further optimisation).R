df <- read.csv('C:/Users/myself/Desktop/R_Analytics/UNI/stats_students.csv')

df <- na.omit(df)# remove na

#Probably better to use payment method for clustering
#df$CSP_IND <- ifelse(df$PAYMENT_MTHD == 'HECS', 'CSP', 'Open Access')
df$Unit_Outcome <- as.character(df$Unit_Outcome) # this is necessary to make unit_outcome levels into 2 after removing withdrawn and not yet graded.
df$ATSI_STATUS <- as.character(df$ATSI_STATUS)
df$ATSI_STATUS <- ifelse(df$ATSI_STATUS == 'NOTABTSI', 'No', 'Yes')
df_model <- subset(df, select = c(SP, PAYMENT_MTHD, Unit_Count, IN_TERM_SUB_STATUS, UNI_ENRL_SOURCE_DESCR, SEX, AGE_Range, ATSI_STATUS, Language, SEC_SCHOOL_COMPLETED, HIGHEST_EDUC, DISABILITY, Father_Edu,Mother_Edu, STATE, UNI_PATH_FLG, IRSADdecile, Study_Goal, Unit_Outcome))
df_model <- subset(df_model, Unit_Outcome %in% c('Failed', 'Passed'))

# Making all the column into factor variables

for (i in 1:dim(df_model)[2]){
  if(class(df_model[,i]) != 'factor'){
    df_model[,i] <- as.factor(df_model[,i])
  }
}

library(klaR)
set.seed(1000)
cluster <- kmodes(df_model[, -22], 3, iter.max=10, weighted=F)
df_model$clust <- as.factor(cluster$cluster)

# For C50, it cannot have an empty factor level. We must add the missing
levels(df_model$SEC_SCHOOL_COMPLETED)[1] <- 'Missing'
levels(df_model$HIGHEST_EDUC)[1] <- 'Missing'
levels(df_model$STATE)[1] <- 'Missing'
levels(df_model$Study_Goal)[1] <-'Missing'

pred_df <- df_model[, -19]
library(caret)

inTrain <- createDataPartition(pred_df$clust, p=0.7, list=F)
training <- pred_df[inTrain,]
testing <- pred_df[-inTrain,]

#######################################################################################
### (1) C50
#######################################################################################

# 1-1. no tuning

library(C50)

modelC50 <- C5.0(clust ~., training)

pred_trainC50 <- predict(modelC50, newdata=training)
confusionMatrix(pred_trainC50, training$clust)

pred_testC50 <- predict(modelC50, newdata=testing)
confusionMatrix(pred_testC50, testing$clust)

# 1-2 with caret package, it does the auto tuning on model, trial and winnow
#     Resampling method is bootstrapped & selection method is 'best'.

modelC50Caret <- train(clust~., training, method="C5.0")
modelC50Caret # check how the model was tuned

pred_trainC50Caret <- predict(modelC50Caret, newdata=training)
confusionMatrix(pred_trainC50Caret, training$clust)

pred_testC50Caret <- predict(modelC50Caret, newdata=testing)
confusionMatrix(pred_testC50Caret, testing$clust)

# 1-3 Add customised tuning methods 

ctrl <- trainControl(method="cv", number=10, selectionFunction = "oneSE")
grid <- expand.grid(.model="tree", .trials=c(1,5,10, 15, 20, 25, 30, 35), .winnow="FALSE")

modelC50Custom <- train(clust~., training, method="C5.0", metric="Kappa", trControl=ctrl, tuneGrid=grid)
modelC50Custom

pred_trainC50Custom <- predict(modelC50Custom, newdata=training)
confusionMatrix(pred_trainC50Custom, training$clust)

pred_testC50Custom <- predict(modelC50Custom, newdata=testing)
confusionMatrix(pred_testC50Custom, testing$clust)

#######################################################################################
### (2) CART
#######################################################################################

library(rpart)

# 2-1. No tuning performed
modelCART <- rpart(clust~., training)

pred_trainCART <- predict(modelCART, newdata=training, type="class")
confusionMatrix(pred_trainCART, training$clust)

pred_testCART <- predict(modelCART, newdata=testing, type="class")
confusionMatrix(pred_testCART, testing$clust)

# 2-2. with caret package, it does the auto tuning to determine the best cp
#     this doesn't perform well. 

modelCARTCaret <- train(clust~., training, method="rpart")
modelCARTCaret # the final value cp = 0.0450...

pred_trainCARTCaret <- predict(modelCARTCaret, newdata=training, type="raw")
confusionMatrix(pred_trainCARTCaret, training$clust)

pred_testCARTCaret <- predict(modelCARTCaret, newdata=testing, type="raw")
confusionMatrix(pred_testCARTCaret, testing$clust)

# 2-3. Custom tuning, instead of bootstrap, use 10 k-fold cross validation
#      This works better than default train for rpart

ctrl <- trainControl(method="cv", number = 10)
grid = expand.grid(.cp = seq(0.01, 0.5, 0.01))

modelCartCustom <- train(clust~., training, method="rpart", trControl=ctrl, tuneGrid=grid)
modelCartCustom # the final value was cp =0.01

pred_trainCartCustom <- predict(modelCartCustom, newdata=training, type="raw")
confusionMatrix(pred_trainCartCustom, training$clust)

pred_testCartCustom <- predict(modelCartCustom, newdata=testing, type="raw")
confusionMatrix(pred_testCartCustom, testing$clust)

#######################################################################################
### (3) bagging
#######################################################################################

# 3-1 no tuning

library(ipred)
set.seed(300)

modelBagging <- bagging(clust~., data=training, nbagg = 25)
modelBagging

pred_trainBagging <- predict(modelBagging, newdata=training)
confusionMatrix(pred_trainBagging, training$clust)

pred_testBagging <- predict(modelBagging, newdata=testing)
confusionMatrix(pred_testBagging, testing$clust)

# 3-2 tuning with caret (ipred tree bagging function is called treebag in caret)

modelBaggingCaret <- train(clust~., data=training, method="treebag")
modelBaggingCaret

pred_trainBagging <- predict(modelBaggingCaret, newdata=training)
confusionMatrix(pred_trainBagging, training$clust)

pred_testBagging <- predict(modelBaggingCaret, newdata=testing)
confusionMatrix(pred_testBagging, testing$clust)

# 3-3 Custom tuning 10 k fold cross validation

ctrl <- trainControl(method="cv", number=10)
modelBaggingCustom <- train(clust ~., data=training, method="treebag", trControl=ctrl)
modelBaggingCustom

pred_trainBaggingCustom <- predict(modelBaggingCustom, newdata=training)
confusionMatrix(pred_trainBaggingCustom, training$clust)

pred_testBaggingCustom <- predict(modelBaggingCaret, newdata=testing)
confusionMatrix(pred_testBaggingCustom, testing$clust)

#######################################################################################
### (4) Random Forest
#######################################################################################

library(randomForest)

modelRF <- randomForest(clust~., data=training, ntree=200)
modelRF

pred_trainModelRF <- predict(modelRF, newdata=training, type="response")
confusionMatrix(pred_trainModelRF, training$clust)

pred_testModelRF <- predict(modelRF, newdata=testing, type="response")
confusionMatrix(pred_testModelRF, testing$clust)

# 4-1 Custom tuning

ctrl <- trainControl(method="repeatedcv", number=10, repeats=10)
grid <- expand.grid(.mtry = c(2, 4, 8, 16, 18))

modelRFCustom <- train(clust~., data=training, method="rf", metric ="Kappa", trControl=ctrl, tuneGrid=grid)
modelRFCustom

pred_trainModelRFCustom <- predict(modelRFCustom, newdata=training, type="raw")
confusionMatrix(pred_trainModelRFCustom, training$clust)

pred_testModelRFCustom <- predict(modelRFCustom, newdata=testing, type="raw")
confusionMatrix(pred_testModelRFCustom, testing$clust)
