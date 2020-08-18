
# Using caret's dummyVar() - it requires at least 2GB memory
# dummyVar() translates text data into numerical data for modeling purposes

library(ISLR); library(caret)
data(Wage)

df <- Wage[, c(2, 3, 4, 5, 6)]
dmy <- dummyVars("~.", data=df)
trsf <- data.frame(predict(dmy, newdata=df))
wage <- cbind(trsf, health=Wage$health)

# Using Feature Hashing - it requires half as much memory as dummyVar()

# one hot encoding


# Reference: 
# dummyVar walkthrough: http://amunategui.github.io/dummyVar-Walkthrough/
# http://amunategui.github.io/feature-hashing/