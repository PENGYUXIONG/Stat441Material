# import data
data(swiss)


# load library
library(MASS)
library(glmnet)

# Using AIC to do variable selection
fullModel = lm(swiss$Fertility ~ swiss$Agriculture + swiss$Examination + swiss$Education + swiss$Catholic + swiss$Infant.Mortality)
AIC <- stepAIC(fullModel)
AIC$anova

# check the relation inside the plot between Catholic and Ferility
plot(swiss$Fertility~swiss$Catholic, pch=16)

# from the plot, I decide to group the datasets into two different groups,
# the 0-20 percentage group and 80 - 100 percentage group
Group1 = swiss[which(swiss$Catholic <= 20),]
Group2 = swiss[which(swiss$Catholic >= 80),]

# data sets
x_1 = Group1[c("Agriculture", "Education", "Catholic", "Infant.Mortality")]
y_1 = Group1$Fertility

x_2 = Group2[c("Agriculture", "Education", "Catholic", "Infant.Mortality")]
y_2 = Group2$Fertility

# training set
x_1_train = head(x_1, 13)
y_1_train = head(y_1, 13)

x_2_train = head(x_2, 8)
y_2_train = head(y_2, 8)

# testing set
x_1_test = tail(x_1, 13)
y_1_test = tail(y_1, 13)

x_2_test = tail(x_2, 8)
y_2_test = tail(y_2, 8)

# multi-regression
regModel1 = lm(y_1_train ~ x_1_train[, 1] + x_1_train[,2] + x_1_train[,3] + x_1_train[,4], data = x_1_train)
regModel2 = lm(y_2_train ~ x_2_train[, 1] + x_2_train[,2] + x_2_train[,3] + x_2_train[,4], data = x_2_train)

# multi-regression testing
multiRegResult1 = predict(regModel1, x_1_test)
multiRegResult2 = predict(regModel2, x_2_test)

AICSSR1 <- sum((multiRegResult1 - y_1_test) ^ 2)
AICSST1 <- sum((y_1_test - mean(y_1_test)) ^ 2)
AICSQR1 <- 1 - AICSSR1 / AICSST1


AICSSR2 <- sum((multiRegResult2 - y_2_test) ^ 2)
AICSST2 <- sum((y_2_test - mean(y_2_test)) ^ 2)
AICSQR2 <- 1 - AICSSR2 / AICSST2

# Ridge regression

# set up testing and training sets
x_1_train = as.matrix(x_1_train)
y_1_train = as.matrix(y_1_train)
x_2_train = as.matrix(x_2_train)
y_2_train = as.matrix(y_2_train)

x_1_test = as.matrix(x_1_test)
y_1_test = as.matrix(y_1_test)
x_2_test = as.matrix(x_2_test)
y_2_test = as.matrix(y_2_test)

# first group model
cv.out = cv.glmnet(x_1_train, y_1_train, alpha = 0, grouped = FALSE)
bestLambda <- cv.out$lambda.min
ridgeModel1 = glmnet(x_1_train, y_1_train, alpha = 0, lambda = bestLambda)
ridgeBeta1 = coef(ridgeModel1)

ridgeResult1 <- predict(ridgeModel1, s = bestLambda, newx = x_1_test)

# second group model
cv.out = cv.glmnet(x_2_train, y_2_train, alpha = 0, grouped = FALSE)
bestLambda <- cv.out$lambda.min
ridgeModel2 = glmnet(x_2_train, y_2_train, alpha = 0, lambda = bestLambda)
ridgeBeta2 = coef(ridgeModel2)

ridgeResult2 <- predict(ridgeModel2, s = bestLambda, newx = x_2_test)

# ridge testing
ridgeSSR1 <- sum((ridgeResult1 - y_1_test) ^ 2)
ridgeSST1 <- sum((y_1_test - mean(y_1_test)) ^ 2)
ridgeSQR1 <- 1 - ridgeSSR1/ridgeSST1

ridgeSSR2 <- sum((ridgeResult2 - y_2_test) ^ 2)
ridgeSST2 <- sum((y_2_test - mean(y_2_test)) ^ 2)
ridgeSQR2 <- 1 - ridgeSSR2/ridgeSST2

# full model
# datasets
x_1 = Group1[c("Agriculture", "Examination", "Education", "Catholic", "Infant.Mortality")]
y_1 = Group1$Fertility

x_2 = Group2[c("Agriculture", "Examination", "Education", "Catholic", "Infant.Mortality")]
y_2 = Group2$Fertility

# training set
x_1_train = head(x_1, 13)
y_1_train = head(y_1, 13)

x_2_train = head(x_2, 8)
y_2_train = head(y_2, 8)

# testing set
x_1_test = tail(x_1, 13)
y_1_test = tail(y_1, 13)

x_2_test = tail(x_2, 8)
y_2_test = tail(y_2, 8)

# multi-regression
regFullModel1 = lm(y_1_train ~ x_1_train[, 1] + x_1_train[,2] + x_1_train[,3] + x_1_train[,4] + x_1_train[,5], data = x_1_train)
regFullModel2 = lm(y_2_train ~ x_2_train[, 1] + x_2_train[,2] + x_2_train[,3] + x_2_train[,4] + x_2_train[,5], data = x_2_train)

# multi-regression testing
FullModelRegResult1 = predict(regFullModel1, x_1_test)
FullModelRegResult2 = predict(regFullModel2, x_2_test)

FullSSR1 <- sum((FullModelRegResult1 - y_1_test) ^ 2)
FullSST1 <- sum((y_1_test - mean(y_1_test)) ^ 2)
FullSQR1 <- 1 - FullSSR1 / FullSST1


FullSSR2 <- sum((FullModelRegResult2 - y_2_test) ^ 2)
FullSST2 <- sum((y_2_test - mean(y_2_test)) ^ 2)
FullSQR2 <- 1 - FullSSR2 / FullSST2

# compare the means
tTestGroup1 = t.test(Group1$Fertility, mu = 67)
tTestGroup2 = t.test(Group2$Fertility, mu = 67)
