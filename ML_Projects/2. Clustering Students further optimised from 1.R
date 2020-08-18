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

set.seed(1000)

#########################################################################
###  Clustering with the K-Modes algorithm.   ###########################
#########################################################################

library(klaR)

# cluster first

cluster <- kmodes(df_model[, -22], 3, iter.max=10, weighted=F)
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
matrix <- as.matrix(table(predict, training$clust))
accuracy_train <- (matrix[1] + matrix[5] + matrix[9])/nrow(training)
accuracy_train
pred_test <- predict(model, newdata=testing)
table(pred_test, testing$clust)
matrix <- as.matrix(table(pred_test, testing$clust))
accuracy_train <- (matrix[1] + matrix[5] + matrix[9])/nrow(testing)
accuracy_train
