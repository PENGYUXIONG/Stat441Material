setwd("./Desktop/stat 441/Assignment5/")
divorce.data = read.table("divorce.data", header=T,na.strings=c(""))

# get data out
divorce.data1 = divorce.data[, c(1, 4, 6, 7)]
divorce.data2 = divorce.data[, c(2, 3, 5)]

# canonical correlation 
cancor.result = cancor(divorce.data1, divorce.data2)


divorce.x1 = as.matrix(divorce.data1) %*% cancor.result$xcoef[, 1]
divorce.x2 = as.matrix(divorce.data1) %*% cancor.result$xcoef[, 2]


divorce.y1 = as.matrix(divorce.data2) %*% cancor.result$ycoef[, 1]
divorce.y2 = as.matrix(divorce.data2) %*% cancor.result$ycoef[, 2]

plot(divorce.x1, divorce.y1, xlab = 'Group1', ylab = 'Group2')
plot(divorce.x2, divorce.y2, xlab = 'Group1', ylab = 'Group2')


library(vegan)
cc3 <- cca(divorce.data1, divorce.data2)
plot(cc3, scaling = 0, xlab = 'Group1', ylab = 'Group2')