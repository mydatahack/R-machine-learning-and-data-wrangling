###################################################################################
## Chose students who took STA10006 and group them into 4 groups by two methods
## Gower distance and k-modes. Then, used nnet, rpart, rf to predict
## It has not dimension reduction done. Used all the variables
###################################################################################

df <- read.csv('stats_students.csv')
df <- as.matrix(df)
df[is.na(df)] <- "No Value" # Make sure to get rid of na. Otherwise modeling doesn't work...

df <- as.data.frame(df)
#Probably better to use payment method for clustering
#df$CSP_IND <- ifelse(df$PAYMENT_MTHD == 'HECS', 'CSP', 'Open Access')
df$Unit_Outcome <- as.character(df$Unit_Outcome) # this is necessary to make unit_outcome levels into 2 after removing withdrawn and not yet graded.
df_model <- subset(df, select = c(SP, PAYMENT_MTHD, Unit_Count, IN_TERM_SUB_STATUS, UNI_ENRL_SOURCE_DESCR, SEX, AGE_Range, ATSI_STATUS, Language, SEC_SCHOOL_COMPLETED, HIGHEST_EDUC, DISABILITY, Country, Father_Edu,Mother_Edu, STATE, UNI_PATH_FLG, IRSADdecile, IERdecile, IEOdecile, Study_Goal, Unit_Outcome))
df_model <- subset(df_model, Unit_Outcome %in% c('Failed', 'Passed'))

# Making all the column into factor variables

for (i in 1:dim(df_model)[2]){
  if(class(df_model[,i]) != 'factor'){
    df_model[,i] <- as.factor(df_model[,i])
  }
}

#########################################################################
### (1) Clustering by gower distance and pam ############################
#########################################################################

# Cluster Group first

library(cluster)
# Dissimilarity matrix calculation

daisy.mat <- as.matrix(daisy(df_model[, -22], metric="gower"))

# Clustering by pam algorithm

my.cluster <- pam(daisy.mat, k=4, diss = T)
df_model$clust <- as.factor(my.cluster$clustering)

# Quick check

table(df_model$clust, df_model$Unit_Outcome)


# 1-1. predict cluster with cart
library(caret)
df_prep <- subset(df_model, select = -c(Unit_Outcome))
inTrain <- createDataPartition(df_prep$clust, p=0.7, list=F)
training <- df_prep[inTrain,]
testing <- df_prep[-inTrain,]

model <- train(clust ~., data = training, method='rpart')
predict <- predict(model, data=training, type="raw")
table(predict, training$clust)
pred_test <- predict(model, newdata=testing, type="raw")
table(pred_test, testing$clust)

# 1-2. predict cluster for test group with nnet

library(nnet)
library(NeuralNetTools)

df_prep <- subset(df_model, select = -c(clust, Unit_Outcome))
dmy <- dummyVars("~.", data=df_prep)
trsf <- data.frame(predict(dmy, newdata=df_prep))
df_nnet_prep <- cbind(trsf, clust=df_model$clust)

inTrain <- createDataPartition(df_nnet_prep$clust, p=0.7, list=F)
train <- df_nnet_prep[inTrain, ]
test <- df_nnet_prep[-inTrain, ]

model <- nnet(clust ~., train, size=4)
#plotnet(model)

# predict train set
# Somehow, this doesn't work...
pred_train <- predict(model, data=train, type="class")
table(train$clust, pred)

pred_test <- predict(model, newdata=test, type="class")
table(test$clust, pred_test)

#########################################################################
### (2) Clustering with the K-Modes algorithm ###########################
#########################################################################

library(klaR)

# cluster first

cluster <- kmodes(df_model[, -22], 4, iter.max=10, weighted=F)
df_model$clust <- as.factor(cluster$cluster)

table(df_model$Unit_Outcome, df_model$clust)

# 1-1. predict cluster with cart
library(caret)
df_prep <- subset(df_model, select = -c(Unit_Outcome))
inTrain <- createDataPartition(df_prep$clust, p=0.7, list=F)
training <- df_prep[inTrain,]
testing <- df_prep[-inTrain,]

model <- train(clust ~., data = training, method='rpart')
predict <- predict(model, data=training, type="raw")
table(predict, training$clust)
pred_test <- predict(model, newdata=testing, type="raw")
table(pred_test, testing$clust)


# 1-2. Random Forest
model <- train(clust ~., data = training, method='rf')
predict <- predict(model, data=training)
table(predict, training$clust)
pred_test <- predict(model, newdata=testing)
table(pred_test, testing$clust)