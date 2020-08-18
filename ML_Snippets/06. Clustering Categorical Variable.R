

# Clustering with the K-Modes algorithm: This only works for categorical variables.
# https://dabblingwithdata.wordpress.com/2016/10/10/clustering-categorical-data-with-r/

library(klaR)
data(Wage)
wage <- subset(Wage, select = -c(year, age, sex, region, logwage, wage))

cluster <- kmodes(wage, 4, iter.max=10, weighted=F)

Wage$clust <- as.factor(cluster$cluster)
ggplot(Wage, aes(x=age, y=wage, color=clust)) + geom_point()




library(ISLR); library(caret); library(nnet); library(NeuralNetTools); library(ggplot2)
library(ClustOfVar)

data(Wage)
wage <- subset(Wage, select = -c(sex, region))

cluster <- kmeansvar(X.quanti=wage[,c(1,2,9,10)],X.quali=wage[,3:8], init=4)

summary(cluster)

wage$clusters <- cluster$clusterCut

ggplot(wage, aes(x=age, y=wage, color=clusters))

# Reference
# Why are mixed data a problem for euclidean-based clustering algorithm?
# http://stats.stackexchange.com/questions/121916/why-are-mixed-data-a-problem-for-euclidean-based-clustering-algorithms/122118#122118

# Culustering Mixed Data Types
# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/

library(dplyr)
library(ISLR)
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2)

set.seed(1680)

data(College)


library(cluster)
data(Wage)
wage <- subset(Wage, select = -c(sex, region))

# Dissimilarity matrix calculation

daisy.mat <- as.matrix(daisy(wage, metric="gower"))

# Clustering by pam algorithm

my.cluster <- pam(daisy.mat, k=4, diss = T)

wage$clust <- as.factor(my.cluster$clustering)

ggplot(data=wage, aes(x=age, y=wage, color=clust)) + geom_point()
# Cluster plot

# clusplot(daisy.mat, diss = T, my.cluster$clustering, color = T)

# Clustering with the K-Modes algorithm
# https://dabblingwithdata.wordpress.com/2016/10/10/clustering-categorical-data-with-r/

library(klaR)
data(Wage)
wage <- subset(Wage, select = -c(year, age, sex, region, logwage, wage))

cluster <- kmodes(wage, 4, iter.max=10, weighted=F)

Wage$clust <- as.factor(cluster$cluster)
ggplot(Wage, aes(x=age, y=wage, color=clust)) + geom_point()
