getwd()

# set the Auto.data file inside the current directory
Auto = read.table("Auto.data", header = TRUE, na.strings = "?", fill = TRUE)
Auto["weight"] = Auto["weight"] / 1000
fix(Auto)

Auto = na.omit(Auto)
data(mtcars)

# check the replations between variables
pairs(~ mpg + displacement + horsepower + weight + cylinders, Auto)

# load the library
# variable selection using AIC
library(MASS)

fullModel = lm(Auto$mpg ~ Auto$cylinders + Auto$displacement +Auto$horsepower + Auto$weight)
AIC <- stepAIC(fullModel)
AIC$anova

# due to the exponential trend of hp and wt that we detected 
# from the scatter plot, they both have exponential correlation
# with mpg, thus, we can applky a linear transformation
# take in the linear model variables we need for regression

autofit = lm(log(mpg) ~ log(horsepower) + log(weight), data = Auto)
AICBeta = coef(autofit)

# lasso model selection
# import library
# please note the package requieres R >= 3.6.0
library(glmnet)

# set up vars
x <- model.matrix(~  cylinders + displacement + horsepower + weight, data = Auto)
y <- Auto$mpg
cv.out = cv.glmnet(x, y, alpha = 1)
bestLambda <- cv.out$lambda.min
lassoModel = glmnet(x, y, alpha = 1, lambda = bestLambda)
lassoBeta = coef(lassoModel)

# test two methods
lassoTestX = model.matrix(~ cyl + disp + hp + wt, mtcars)
AICTestX = mtcars[c("hp", "wt")]
names(AICTestX)[1] <- "horsepower"
names(AICTestX)[2] <- "weight"
# AIC
AICresult <- exp(predict(autofit,  newdata = AICTestX))

# lasso
lassoResult <- predict(lassoModel, s = bestLambda, newx = lassoTestX)

# R square for lasso Model
LassoSSR <- sum((lassoResult - mtcars$mpg) ^ 2)
LassoSST <- sum((mtcars$mpg - mean(lassoResult)) ^ 2)
LassoSQR <- 1 - LassoSSR/LassoSST

# R square for AIC linear regression model
AICSSR <- sum((AICresult - mtcars$mpg) ^ 2)
AICSST <- sum((mtcars$mpg - mean(AICresult)) ^ 2)
AICSQR <- 1 - AICSSR / AICSST



