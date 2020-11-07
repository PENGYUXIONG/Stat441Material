setwd("./Desktop/stat 441/Assignment4/")

library("tree")
library("e1071")
library("gbm")

# data processing
diabetes.data = read.table("diabetes.data", header=T,na.strings=c(""))

cols_to_imp = c("plas","pres","skin","insu","mass", "pedi", "age")
diabetes.data[cols_to_imp][diabetes.data[cols_to_imp] == 0] = NA

diabetes_col = colMeans(diabetes.data[, cols_to_imp], na.rm=TRUE)
for(i in 1:ncol(diabetes.data)) {
  diabetes.data[ , i][is.na(diabetes.data[ , i])] = as.integer(mean(diabetes.data[ , i], na.rm = TRUE))
}

# cross validation
# validation size = 168 training size = 600
# randomly select 100 samples randomly for each test batch, 10 batches in total.
# choose the batch that has best performance
set.seed(1)
train.index = sample (1: nrow(diabetes.data ), 600)
diabetes.train = diabetes.data[train.index, ]
diabetes.val = diabetes.data[-train.index, ]


# simple tree classification 
diabetes.tree = tree(as.factor(positive)~. -positive, diabetes.data, subset = train.index)
plot(diabetes.tree)
text(diabetes.tree, pretty = 0)


# tree pruning block using simple tree model
prune.tree =prune.misclass (diabetes.tree, best = 8 )

# cross validation
# k-fold 10 epochs
classify_tree_current_acc = 0
prune_tree_current_acc = 0
for (t in 1:10){
  # set batch size to 100
  val_batch_index = sample(1:nrow(diabetes.val), 100)
  val_batch = diabetes.val[val_batch_index, ]
  
  tree.pred=predict(diabetes.tree, val_batch,type ="class")
  prune_tree.pred = predict(prune.tree, val_batch, type = "class")
  
  val_batch.test = val_batch[, 9]
  table(tree.pred, val_batch.test)
  classify_tree_current_acc = mean(tree.pred == val_batch.test) + classify_tree_current_acc
  
  table(prune_tree.pred, val_batch.test)
  prune_tree_current_acc = mean(prune_tree.pred == val_batch.test) + prune_tree_current_acc
}

actual_classify_tree_acc = classify_tree_current_acc / 10
actual_prune_tree_acc = prune_tree_current_acc / 10


# boosting model

set.seed(1)
diabetes.boost = gbm(positive~.,data=diabetes.train, shrinkage=0.01, distribution= "bernoulli"
                     ,n.trees =6000 , interaction.depth = 4, cv.folds = 10, verbose = F)

best.iter = gbm.perf(diabetes.boost, method="cv")
summary(diabetes.boost)

yhat.boost = predict(diabetes.boost, newdata = diabetes.val, n.trees = 6000, type = "link")
mean((yhat.boost - diabetes.val$positive)^2) 

prediction_binary = as.factor(ifelse(yhat.boost > 0.5, 1, 0 ))
ground_T = as.factor(diabetes.val$positive)
boosting_acc = mean(prediction_binary == ground_T)

# svm Model
tune.out= tune(svm, positive~. ,data=diabetes.train, kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100, 0.00001)) )

svmfit = svm( positive~., data=diabetes.train , type = "C-classification", kernel ="linear", cost =0.01)

prediction = predict(svmfit, diabetes.val[, -9])
svm_acc = mean(prediction == diabetes.val$positive)

plot(svmfit, diabetes.val, mass~plas)

