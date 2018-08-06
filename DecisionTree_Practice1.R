install.packages('rpart')
library(rpart)


str(kyphosis)
head(kyphosis)  
kk<-kyphosis



tree <- rpart(Kyphosis ~ ., method = "class", data=kk)

printcp(tree)

plot(tree,uniform = T, main= 'Kyphosis Tree')

text(tree, use.n = T, all =  T)


install.packages('rpart.plot')

library(rpart.plot)

prp(tree)


install.packages('randomForest')
library(randomForest)

rf.model<- randomForest(Kyphosis ~ ., data=kk)
print(rf.model)

rf.model$confusion


#################################################
#project
#################################################


library(ISLR)
str(College)
head(College)


library(car) 
plot(College$Room.Board, College$Grad.Rate, col=College$Private, xlab="Room.Board", ylab="Grad.Rate", pch=1)
legend(x="right", legend = levels(College$Private), col = c("black","red"),pch=1)


ggplot(data= College, aes(x=Room.Board, y=Grad.Rate, color=Private))+geom_point()+theme(legend.position="right")

qplot(F.Undergrad, data=College,geom="histogram", fill=Private, col=I("black"))+theme(legend.position="right")


ggplot(College, aes(x=F.Undergrad, fill=Private, order=length(Private), col=I("black")))+ 
  geom_histogram(position="identity")+ #identity position is putting the other group at front
  theme(legend.position="right") 
  
  
ggplot(College, aes(F.Undergrad))+
  geom_histogram(aes(fill=Private),position=position_stack(reverse=TRUE), color="black", bins=50, alpha=0.5)+
  theme_bw()


ggplot(College, aes(Grad.Rate))+
  geom_histogram(aes(fill=Private),position=position_stack(reverse=TRUE), color="black", bins=50, alpha=0.5)+
  theme_bw()

College[College$Grad.Rate>100,]

sample <- College

sample$Grad.Rate <- ifelse(sample$Grad.Rate >100, 100, sample$Grad.Rate)
sample[sample$Grad.Rate>100,]


set.seed(101)
library(caTools)
library(base)
library(rpart)
library(rpart.plot)

split <- sample.split(sample$Private, SplitRatio=.7)

sample.train <- subset(sample, split==T)
sample.test <- subset(sample, split==F)


tree2 <- rpart(Private ~ ., method = "class", data=sample.train)

printcp(tree2)

plot(tree2,uniform = T, main= 'Private Tree')

text(tree2, use.n = T, all =  T)


prp(tree2)


#predict test data
table(sample.test$Private,predict(tree2, sample.test, type='class'))

error.rate <- sum(sample.test$Private != predict(tree2, sample.test, type='class'))/nrow(sample.test)




#random forest

test.model<- randomForest(Private ~ ., data=sample.train,  importance=TRUE)
print(test.model)

test.model$confusion

test.model$importance


table(sample.test$Private,predict(test.model, sample.test, type='class'))

