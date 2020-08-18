########################################################################
################### SparkR Introduction ################################
########################################################################

## (1) Set environment (Start Spark R from RStudio)
##     SPARK_HOME has to be set as environment variable
print(Sys.getenv("SPARK_HOME")) # check if SPARK_HOME is set

# Then specify the libray path and start the sparkR session
library(SparkR, lib.loc = "C:\\spark\\R\\lib")
# check native library path with .libPath() and add to extra library path
sparkR.session(spark.driver.extraLibraryPath = c("C:/Users/myself/Documents/R/win-library/3.3", "C:/Program Files/R/R-3.3.2/library"))

df <- as.DataFrame(faithful)
head(df)

library(dplyr)

install.packages("dplyr")

df <- as.DataFrame(mtcars)
printSchema(df)
type(df)

## (2) Read and Write file operations

people <- read.df("./examples/src/main/resources/people.json", "json")

# Similarly, multiple files can be read with read.json
people <- read.json(c("./examples/src/main/resources/people.json", "./examples/src/main/resources/people2.json"))

# csv example
df <- read.df(csvPath, "csv", header = "true", inferSchema = "true", na.strings = "NA")

# write example
write.df(people, path = ".../people.parquet", source = "parquet", mode = "overwrite")
write.df(people, path = ".../people.parquet", source = "csv", mode = "overwrite")

## (3) Use sql
# Registering temp view - registerTempTable is depricated in 2.x (however, backward-compatible)
df <- as.DataFrame(mtcars)
createOrReplaceTempView(df, "mtcars")
check <- sql("Select * From mtcars where cyl = 6")
head(check)

## (4) Operations
head(select(df,df$mpg))
head(filter(df, df$mpg < 20))
head(summarize(groupBy(df, df$mpg), sum = sum(df$mpg)))
head(summarize(groupBy(df, df$mpg), average = avg(df$hp)))

## (5) Linear Regression
#      Create train & test set: use randomSplit in Spark 2
trainTest <- randomSplit(df, c(0.7, 0.3), seed = 101)
train = trainTest[[1]]
test = trainTest[[2]]
# create a model
modelLR <- SparkR::glm(mpg ~ hp + cyl, data = train, family = "gaussian")  #SparkR:: is optional
SparkR::summary(modelLR)
# Check Model on training set
predLR <- SparkR::predict(modelLR, newData = train)
head(predLR)

predLR_test <- SparkR::predict(modelLR, newData = test)
head(predLR_test)

## (6) Logistic Regression

modelLog <- SparkR::glm(am ~ hp + mpg + wt, data = train, family = "binomial")
SparkR::summary(modelLog)

pred_train <- SparkR::predict(modelLog, newData = train)
head(pred_train)

# Use 0.5 for cutoff (reference p51 Green R note)
# Example of using user defined function with dapplyCollect on dataframe.

convert_to_class <- function(df){
  df$class_pred <- ifelse(df$prediction >= 0.5, 1, 0)
  df
}

converted <- dapplyCollect(
                            pred_train, 
                            convert_to_class
                            )
head(converted)

########################################################################
################### SparkR Introduction ################################
########################################################################

# (1) Data prep

df <- read.df("/FileStore/tables/vztl72t11495708041348/stats_students.csv", "csv", header = T)
df <- na.omit(df) # not sure if this is working. Need to check
head(df)
printSchema(df)
print(describe(df))

# (2) use Spark SQL

createOrReplaceTempView(df, "student")

new_df = sql("Select SP, PAYMENT_MTHD, Unit_Count, IN_TERM_SUB_STATUS, OUA_ENRL_SOURCE_DESCR, SEX, AGE_Range, ATSI_STATUS, Language, SEC_SCHOOL_COMPLETED, HIGHEST_EDUC, DISABILITY, Father_Edu,Mother_Edu, STATE, OUA_PATH_FLG, IRSADdecile, Study_Goal, Unit_Outcome
             From student Where Unit_Outcome In ('Passed', 'Failed')")

SparkR::head(new_df)

# (3) Prediction with Random Forest

# sparkR.session() # this is not needed for databricks
sparkR.session(sparkPackages = "com.databricks:spark-avro_2.11:3.0.0") # not sure if this is needed

df <- read.df("/FileStore/tables/vztl72t11495708041348/stats_students.csv", "csv", header = T)
df <- na.omit(df)

createOrReplaceTempView(df, "student")

new_df = sql("Select SP, PAYMENT_MTHD, Unit_Count, IN_TERM_SUB_STATUS, OUA_ENRL_SOURCE_DESCR, SEX, AGE_Range, ATSI_STATUS, Language, SEC_SCHOOL_COMPLETED, HIGHEST_EDUC, DISABILITY, Father_Edu,Mother_Edu, STATE, OUA_PATH_FLG, IRSADdecile, Study_Goal, Unit_Outcome
From student Where Unit_Outcome In ('Passed', 'Failed')")

SparkR::head(new_df)

################## Random Forest ########################

# (1) train test dataset prep

trainTest <- randomSplit(new_df, c(0.7, 0.3), seed=101)
train = trainTest[[1]]
test = trainTest[[2]]

modelRF<- spark.randomForest(Unit_Outcome ~ SP + PAYMENT_MTHD + Unit_Count + IN_TERM_SUB_STATUS +  OUA_ENRL_SOURCE_DESCR + SEX + AGE_Range + SEC_SCHOOL_COMPLETED + HIGHEST_EDUC + Father_Edu + Mother_Edu, data = train, "classification", numTrees = 10)

predict_train <- predict(modelRF, train)

showDF(predict_train)

dfR_train <- collect(predict_train)
tab <- table(dfR_train$Unit_Outcome,  dfR_train$prediction)

print(tab)

accuracy <- function(table){
  round(((table[1,1] + table[2,2])/ (table[1,1] + table[1,2] + table[2,1] + table[2,2])), 3)
}

sensitivity <- function(table){
  round(table[2, 2]/(table[1, 1] + table[2, 1]), 3)
}

specificity <- function(table){
  round(table[1, 1]/(table[1, 1] + table[1, 2]), 3)
}

print(accuracy(tab))
print(sensitivity(tab)) # true positive rate
print(specificity(tab)) # true negative rate

# Use test set

# test prediction

predict_test <- predict(modelRF, test)
dfR_test <- collect(predict_test)
tab <- table(dfR_test$Unit_Outcome,  dfR_test$prediction)

print(accuracy(tab))
print(sensitivity(tab)) # true positive rate
print(specificity(tab)) # true negative rate
