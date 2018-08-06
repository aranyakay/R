library(ISLR)
print(head(iris))

library(e1071)

model<- svm(Species ~ ., data=iris)
summary(model)

pred.values <- predict(model, iris[1:4])
table(pred.values,iris[,5])


#let r choose gammer and cost value
tune.results <- tune(svm, train.x = iris[1:4], 
                     train.y =iris[,5], kernel='radial')
summary(tune.results)

tune.results <- tune(svm, train.x = iris[1:4], 
                     train.y =iris[,5], kernel='radial',
                     ranges = list(cost=rep(.5:2,4),gamma=rep(.1:1,10)))

summary(tune.results)


#when cost=1.5, gamma=.1

tuned.svm <-svm(Species ~ ., data = iris, kernel = 'radial',
                cost=1.5, gamma=.1)
summary(tuned.svm)


#project begin
#data from lending club


lending <- read.csv("C:/Users/jchen/Downloads/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/loan_data.csv")

head(lending)
str(lending)
summary(lending)

# lending$inq.last.6mths <- as.factor(lending$inq.last.6mths)
# lending$delinq.2yrs <- as.factor(lending$delinq.2yrs)
# lending$pub.rec <- as.factor(lending$pub.rec)
# lending$not.fully.paid <- as.factor(lending$not.fully.paid)
# lending$credit.policy  <- as.factor(lending$credit.policy)



#ggplot


ggplot(lending, aes(fico))+
  geom_histogram(aes(fill=not.fully.paid), position=position_stack(reverse=TRUE),color="black",bins=40)+
  scale_fill_manual(values = c('green','red'))+
  theme_bw()



ggplot(lending, aes(purpose))+
  geom_bar(aes(fill=not.fully.paid), position='dodge',color="black",bins=20)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Create a scatterplot of fico score versus int.rate

ggplot(lending, aes(int.rate,fico))+
         geom_point(aes(color=not.fully.paid))+
  theme_bw()


#Split your data into training and test sets using the caTools library
library(caTools)
set.seed(101)

split <- sample.split(lending$not.fully.paid, SplitRatio=.7)

sample.train <- subset(lending, split==T)
sample.test <- subset(lending, split==F)


#Call the e1071 library as shown in the lecture.
library(e1071)

#Now use the svm() function to train a model on your training set.
pmod<- svm(not.fully.paid ~ ., data=sample.train)


#Use predict to predict new values from the test set using your model.
table(sample.test$not.fully.paid,predict(pmod, sample.test, type='class'))


#Use the tune() function to test out different cost and gamma values.

var <- as.matrix(lending[,1:13])
y <- as.factor(lending$not.fully.paid)

lending$purpose<- as.numeric(lending$purpose)
tune.project <- tune(svm, train.x= lending[,1:13], train.y=lending[,14],
                    kernel='radial', ranges = list(cost=rep(1:10,10),gamma=rep(.1:1,10)))








str(lending)



#support vector machines project

library(ISLR)
head(iris)

library(e1071)

data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- iris$Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

## try regression mode on two dimensions

# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- svm(x, y)
new <- predict(m, x)

# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

## density-estimation

# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

# traditional way:
m <- svm(X, gamma = 0.1)

# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

# weights: (example not particularly sensible)
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)

