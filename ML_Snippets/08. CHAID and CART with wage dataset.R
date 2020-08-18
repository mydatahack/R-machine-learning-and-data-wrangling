# Reference: http://www.rpubs.com/zulhanif/228644
#   CHAID needs to have factors as predictors. When the outcome classes are multiple and unbalanced,
# Both Chaid and Cart, small classes won't end up in the last nodes as they are overpowered by 
# the majority.

# (1) Package installation
install.packages("CHAID", repos="http://R-Forge.R-project.org")

library(ISLR)
library(CHAID)

# (2) Data Wrangling
data(Wage)
df = Wage

hist(df$wage)

df$wage_class <- ifelse(df$wage < 60, 'Below 60',
                        ifelse(df$wage >= 60 & df$wage <100, '50-100',
                               ifelse(df$wage >= 100 & df$wage < 200, '100-200',
                                      ifelse(df$wage >= 200, "Over 200", 'Unclassified'))))
df$age_range <- ifelse(df$age <20, 'Below 20',
                       ifelse(df$age >=20 & df$age < 30, '20-30',
                              ifelse(df$age >=30 & df$age < 40, '30-40',
                                     ifelse(df$age >= 40 & df$age < 50, '40-50',
                                            ifelse(df$age >= 50 & df$age <= 60, '50-60', 'Over 60')))))
# Note: all the independent variables have to be factors otherwise get error: is.factor(x) is not TRUE
df$wage_class = as.factor(df$wage_class)
df$age_range <- as.factor(df$age_range)

table(df$wage_class)

df2 <- subset(df, select= -c(year, logwage, wage, age))

# (3) Create a model
dt.chaid <- chaid(wage_class ~., 
                  control = chaid_control(minprob = 0.01, minsplit = 200,minbucket = 30)
                  ,data=df2)
# (4) plot
plot(dt.chaid, 
     uniform = T, 
     compress = T, 
     margin = 0.2, 
     branch = 0.3)

# This doesn't work. 
text(dt.chaid, 
     use.n = T, 
     digits = 3, 
     cex = 0.6)

summary(dt.chaid)

# (5) Cart and plot. rpart.plot() is better than prp()
library(rpart)
library(rpart.plot)
mod <- rpart(wage_class ~., data=df2, method="class")
rpart.plot(mod)

######################################
# Binary Classification ##############
######################################
df$wage_class <- ifelse(df$wage < 100, 'Below 100',
                        ifelse(df$wage >= 100, 'Over 100', 'Unclassified'))
df$wage_class = as.factor(df$wage_class)
df2 <- subset(df, select= -c(year, logwage, wage, age))

table(df2$wage_class)

dt.chaid <- chaid(wage_class ~., 
                  control = chaid_control(minprob = 0.001, minsplit = 200,minbucket = 700)
                  ,data=df2)

plot(dt.chaid, 
     uniform = T, 
     compress = T, 
     margin = 0.2, 
     branch = 0.3)

text(dt.chaid, 
     use.n = T, 
     digits = 3, 
     cex = 0.6)

summary(dt.chaid)



# Cart with Binary Classification
mod <- rpart(wage_class ~., data=df2, method="class")
rpart.plot(mod)
