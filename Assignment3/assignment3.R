# fit the data into model
#############################################
logistics_regression <- function(index, skull.test){
  # logistic regression for skull dataset
  glm.fits = glm(formula = Type ~ Length + Breadth + Height + Fheight + Fbreadth, data = skull, subset = -index , family = binomial)
  
  
  # predict 
  glm.probs =predict (glm.fits, newdata = skull.test, type ="response")
  
  glm.pred=rep (0 ,1)
  glm.pred[glm.probs >.5]= 1
  
  table(glm.pred, skull.test$Type)
  
  skull_lr_result = mean(glm.pred== skull.test$Type )
  skull_lr_error = 1 - skull_lr_result
  return(skull_lr_result)
}

#############################################
# knn
KNN <- function(index, skull.test){
  train.X=cbind(Length = skull$Length, Breadth = skull$Breadth, 
                Height = skull$Height, Fheight = skull$Fheight,
                Fbreadth = skull$Fbreadth)[-index ,]
  test.X=cbind(Length = skull$Length, Breadth = skull$Breadth, 
                Height = skull$Height, Fheight = skull$Fheight,
                Fbreadth = skull$Fbreadth)[index ,]
  classification.X = cbind(Type = skull$Type)[-index ,]
  
  knn.pred = knn(train = train.X, test = test.X, cl = classification.X, k = 3, prob = T)

  table(knn.pred, skull.test$Type)

  skull_knn_result = mean(knn.pred== skull.test$Type)
  skull_knn_error = 1 - skull_knn_result
  return(skull_knn_result)
}

###############################################
# LDA
LDA <- function(index, skull.test){
  lda.fit = lda(formula = Type ~ Length + Breadth + Height + Fheight + Fbreadth, data = skull, subset = -index)
  lda.pred=predict (lda.fit , skull.test)
  print(table(as.matrix(lda.pred$class) , skull.test$Type))
  
  skull_LDA_result = mean(lda.pred$class== skull.test$Type)
  skull_LDA_error = 1 - skull_LDA_result
  
  return(skull_LDA_result)
}

#################################################
# QDA

QDA <- function(index, skull.test){
  qda.fit = qda(formula = Type ~ Length + Breadth + Height + Fheight + Fbreadth, data = skull, subset = -index)
  qda.pred=predict (qda.fit , skull.test)
  table(qda.pred$class , skull.test$Type)
  
  skull_QDA_result = mean(qda.pred$class== skull.test$Type)
  skull_QDA_error = 1 - skull_QDA_result
  return(skull_QDA_result)
}


library("MASS")
library("class")
library("ggplot2")

# Dataset 1
########################################################
# data processing
skull = read.table("tibetSkull.data", header = TRUE, na.strings = "?", fill = TRUE, row.names = 1)
# make the label binary data
skull[, 6] = skull[,6] -1

skull_lr_result = 0
skull_knn_result = 0
skull_LDA_result = 0
skull_QDA_result = 0

# separate data into training and testing datasets
for (index in 1:32){
  skull.train = skull[-index,]
  skull.test = skull[index,]
  skull_lr_result = logistics_regression(index, skull.test[-6, ]) + skull_lr_result
  skull_knn_result = KNN(index, skull.test[-6, ]) + skull_knn_result
  skull_LDA_result = LDA(index, skull.test[-6, ]) + skull_LDA_result
  skull_QDA_result = QDA(index, skull.test[-6, ]) + skull_QDA_result
}

skull_lr_result = skull_lr_result / 32
skull_knn_result = skull_knn_result / 32
skull_LDA_result = skull_LDA_result / 32
skull_QDA_result = skull_QDA_result / 32

# check the distance mapping of the features
skull.dist = sammon(dist(skull))
plot(skull.dist[[1]],pch=16)

#plot

ldaModel = predict(lda.fit)
plot(ldaModel$x[, 1], ylab = "coe of LDA")

ggplot(skull.train)+geom_point(aes(1:31, ldaModel$x[, 1], colour = Type), size = 2.5) + xlab("index") + ylab("lda model coe")

