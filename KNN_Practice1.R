#install.packages("ISLR")
library(ISLR)
str(Caravan)

summary(Caravan$Purchase)
any(is.na(Caravan))
var(Caravan$MOSTYPE)

purchase <- Caravan[,86]
st.Caravan <- scale(Caravan[,-86])


#Train Test Split
test.index<-1:1000
test.data<-st.Caravan[test.index,]
test.purchase <- purchase[test.index]


#Train
train.data<- st.Caravan[-test.index,]
train.purchase<-purchase[-test.index]



########
#KNN
########

library(class)
set.seed(101)

predicted.purchase<- knn(train.data,test.data,train.purchase, k=1)

print(head(predicted.purchase))

misclass.error <- sum(test.purchase != predicted.purchase)


#######################################
#test for different K value
###################################
predicted.purchase<- knn(train.data,test.data,train.purchase, k=5)
misclass.error <- sum(test.purchase != predicted.purchase)



#loop test
predicted.purchase <- NULL
error.rate <- NULL

for(i in 1:20) { 
  set.seed(101)
  predicted.purchase <- knn(train.data,test.data,train.purchase, k=i)
  error.rate[i] <- sum(test.purchase != predicted.purchase) 
   }

error.rate



#################################
#Visualize K elbow method
#################################

library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate,k.values)
ggplot(error.df, aes(k.values, error.rate)) + geom_point() +geom_line(lty = 'dotted', color='red')





########################################################################
#############################################################################
#######################################################################################

#KNN Project
#install.packages("ISLR")

head(iris)

str(iris)
iris2 <- iris[,-5]
Species <- iris$Species

st.iris <- scale(iris2)
st.iris <- as.data.frame(st.iris)


var(st.iris[,1])

st.iris$Species <- Species


set.seed(101)
rr<-runif(45,1,150)

iris.test <- st.iris[rr,1:4]
species.test <- st.iris[rr,5]
iris.train <- st.iris[-rr,1:4]
species.train <- st.iris[-rr,5]


library(class)
species.knn <- knn(iris.train,iris.test,species.train,k=1)

species.knn

error.rate <- sum(species.knn != species.test)/45


for(i in 1:10) { 
  set.seed(101)
  species.knn <- knn(iris.train,iris.test,species.train, k=i)
  error.rate[i] <- sum(species.knn != species.test)/45 
}

error.rate

k.values<-1:10
error.df <- data.frame(k.values, error.rate)
ggplot(error.df, aes(k.values, error.rate)) + geom_point() +geom_line(lty = 'dotted', color='red')



