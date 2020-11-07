library("MASS")
library("class")
library("ggplot2")


# Dataset 2
#############################################

# data processing
diabetes.data = read.table("diabetes.data", header=T,na.strings=c(""))

cols_to_imp = c("plas","pres","skin","insu","mass", "pedi", "age")
diabetes.data[cols_to_imp][diabetes.data[cols_to_imp] == 0] = NA



diabetes_col = colMeans(diabetes.data[, cols_to_imp], na.rm=TRUE)
for(i in 1:ncol(diabetes.data)) {
  diabetes.data[ , i][is.na(diabetes.data[ , i])] = as.integer(mean(diabetes.data[ , i], na.rm = TRUE))
}

# check the distance mapping of the features
diabetes.dist = sammon(dist(diabetes.data))
plot(diabetes.dist[[1]],pch=16, xlab = "CP1", ylab = "CP2")

# cross validation
# validation size = 168 training size = 600
# randomly select 100 samples randomly for each test batch, 10 batches in total.
# choose the batch that has best performance
set.seed(1)
diabetes.train = diabetes.data[1:600, ]
diabetes.val = diabetes.data[601:768, ]

# Logistic regression model training
glm.fit = glm(formula = positive ~ preg + plas + pres + skin + insu + mass + pedi + age, data = diabetes.data, subset = 601:768, family = binomial )

# knn
train.X=cbind(preg = diabetes.data$preg, plas = diabetes.data$plas, 
              pres = diabetes.data$pres, skin = diabetes.data$skin,
              insu = diabetes.data$insu, mass = diabetes.data$mass,
              pedi = diabetes.data$pedi, age = diabetes.data$age)[1:450 ,]
val.X=cbind(preg = diabetes.data$preg, plas = diabetes.data$plas, 
              pres = diabetes.data$pres, skin = diabetes.data$skin,
              insu = diabetes.data$insu, mass = diabetes.data$mass,
              pedi = diabetes.data$pedi, age = diabetes.data$age)[451:600 ,]
test.X=cbind(preg = diabetes.data$preg, plas = diabetes.data$plas, 
            pres = diabetes.data$pres, skin = diabetes.data$skin,
            insu = diabetes.data$insu, mass = diabetes.data$mass,
            pedi = diabetes.data$pedi, age = diabetes.data$age)[601:768 ,]
classification.X = cbind(positive = diabetes.data$positive)[1:450 ,]
best_diabetes_knn_result = 0
best_k = NULL
# cross_val tuning
# 450 training and 150 validation, 168 testing
# tuning k from 1 to 100
for(iter in 1:100){
  knn.pred = knn(train = train.X, test = val.X, cl = classification.X, k = iter, prob = T)
  diabete_knn_result = mean(knn.pred== diabetes.data[451:600, ]$positive)
  if(diabete_knn_result > best_diabetes_knn_result){
    best_diabetes_knn_result = diabete_knn_result
    best_k = iter
  }
}


# lda
lda.fit = lda(formula = positive ~ preg + plas + pres + skin + insu + mass + pedi + age, data = diabetes.data, subset = 601:768 )

# qda
qda.fit = qda(formula = positive ~ preg + plas + pres + skin + insu + mass + pedi + age, data = diabetes.data, subset = 601:768 )


# for knn  best k = 37, with 83.3333 percents accuray in validation
# cross validation --- test
diabetes_lr_result = 0
diabete_knn_result = 0
diabetes_LDA_result = 0
diabetes_QDA_result = 0
for (t in 1:10){
  index = sample(1:168, 100)
  diabetes.val_batch = diabetes.val[index, ]
  
  # logistic regression validation
  glm.probs =predict (glm.fit, newdata = diabetes.val_batch[-9], type ="response")
  glm.pred=rep (0 ,100)
  glm.pred[glm.probs >.5]= 1
  table(glm.pred, diabetes.val_batch$positive)
  diabetes_lr_result = mean(glm.pred== diabetes.val_batch$positive ) + diabetes_lr_result
  
  #knn
  knn.pred = knn(train = train.X, test = test.X[index,], cl = classification.X, k = 37, prob = T)
  table(knn.pred, diabetes.val_batch$positive)
  diabete_knn_result = mean(knn.pred== diabetes.val_batch$positive) + diabete_knn_result
  
  # lda
  lda.pred = predict(lda.fit, diabetes.val_batch[-9])
  print(table(lda.pred$class, diabetes.val_batch$positive))
  diabetes_LDA_result = mean(lda.pred$class== diabetes.val_batch$positive) + diabetes_LDA_result
  
  # qda
  qda.pred = predict(qda.fit, diabetes.val_batch[-9])
  diabetes_QDA_result = mean(qda.pred$class== diabetes.val_batch$positive) + diabetes_QDA_result
}

diabetes_lr_result = diabetes_lr_result / 10
diabete_knn_result = diabete_knn_result/ 10
diabetes_LDA_result = diabetes_LDA_result / 10
diabetes_QDA_result = diabetes_QDA_result / 10




# LDA plot
lda.fit = lda(formula = positive ~ preg + plas + pres + skin + insu + mass + pedi + age, data = diabetes.data)
ldaModel = predict(lda.fit)
plot(ldaModel$x[, 1], ylab = "coe of LDA")

ggplot(diabetes.data)+geom_point(aes(1:768, ldaModel$x[, 1], colour = positive), size = 2.5) + xlab("index") + ylab("lda model coe")



  