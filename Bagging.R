library(MASS)

install.packages("randomForest")
library(randomForest)
dim(Boston) ; fix(Boston)
train = sort(sample(1:nrow(Boston),nrow(Boston)*0.6))
test= sort(setdiff(1:nrow(Boston),train))

set.seed(511)
bag.boston = randomForest(medv~.,data=Boston,subset=train,mtry=13)
bag.boston
yhat.bag = predict(bag.boston,Boston[test,])
y = Boston[test,]$medv
plot(yhat.bag,y)
abline(0,1)
mean((y-yhat.bag)^2)

bag.boston2 = randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
bag.boston2
yhat.bag2 = predict(bag.boston2,Boston[test,])
mean((y-yhat.bag2)^2)
