setwd("./Desktop/stat 441/Assignment4/")

library("rpart")
library("ipred") 
library("tree")
library("e1071")
library("gbm")

oil.data = read.table("crude_oil.data", header=T,na.strings=c(""))

# data size is only 56, split into test and training set
set.seed(1)
train.index = sample (1: nrow(oil.data ), 40)
oil.train = oil.data[train.index, ]
oil.test = oil.data[-train.index, ]

# simple tree classification
oil.tree = tree(as.factor(unit)~. , oil.data, subset = train.index)
# classification tree model has 4 nodes
plot(oil.tree)
text(oil.tree, pretty = 0)

# tree pruning 
prune.tree =prune.misclass (oil.tree ,best = 4)
plot(prune.tree)
text(prune.tree, pretty = 0)

tree.pred=predict(oil.tree, oil.test,type ="class")
prune_tree.pred = predict(prune.tree, oil.test, type = "class")

Classifcation_tree_acc = mean(tree.pred == oil.test$unit)
prune_tree_acc = mean(prune_tree.pred == oil.test$unit)

# bagging
oil.bagging = bagging(unit~. , data = oil.train,nbagg = 100, coob = T, 
                      control = rpart.control(minsplit = 2), cp = 0)
oil_bag_pred = predict(oil.bagging, newdata = oil.test)
bagging_acc = mean(oil.test$unit == oil_bag_pred)

# svm model
tune.out= tune(svm, unit~. ,data=oil.train, kernel ="linear",
               ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100, 0.00001)) )

# best cost = 1
svmfit = svm( unit~., data=oil.train , type = "C-classification", kernel ="linear", cost =1)
svm_prediction = predict(svmfit, oil.test[, -6])
svm_acc = mean(svm_prediction == oil.test$unit)

plot(svmfit, oil.test, x4~x4.1)
plot(svmfit, oil.test, x4~x1)

