#K means clustering 
#Unsupervised learing algorithms
library(ISLR)
library(ggplot2)

head(iris)

ggplot(iris, aes(Petal.Length, Petal.Width, color=Species))+geom_point(size=4)



set.seed(101)

iris.cluster <- kmeans(iris[, 1:4], 3, nstart=20)

table(iris.cluster$cluster, iris$Species)



#visulization clusters
library(cluster)
clusplot(iris, iris.cluster$cluster, color=T, shade=T, label=0, line=0)



################################################################################
#K Means clustering project begins
################################################################################



downloads <- "C:/Users/jchen/Downloads/R-Course-HTML-Notes/"

red.wine <- read.csv(paste(downloads, 'winequality-red.csv', sep=""), sep=";")
white.wine <- read.csv(paste(downloads, 'winequality-white.csv', sep=""), sep=";")

red.wine$label <- "red"
white.wine$label <- "white"


head(red.wine)
head(white.wine)


wine <- rbind(red.wine, white.wine)

str(wine)

ggplot(wine, aes(residual.sugar, fill=label)) + geom_histogram(bins=30,color='black')+
  scale_fill_manual(values=c("red", "white")) #adjust color fits the wine's color


ggplot(wine, aes(citric.acid, fill=label)) + geom_histogram(bins=30,color='black')+
  scale_fill_manual(values=c("red", "white")) #adjust color fits the wine's color


ggplot(wine, aes(alcohol, fill=label)) + geom_histogram(bins=30,color='black')+
  scale_fill_manual(values=c("red", "white")) #adjust color fits the wine's color


ggplot(wine, aes(citric.acid, residual.sugar, color=label)) + geom_point(shape=1, size=4)


clus.data <- wine[, 1:12]


#call K means with K=2
set.seed(101)
wine.clus <- kmeans(clus.data, 2, nstart = 20)

table(wine.clus$cluster, wine$label)









