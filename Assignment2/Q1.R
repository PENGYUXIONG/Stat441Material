# please make sure the data file is at the current directory

# read in data
cigs = read.table("cigs.data", header = TRUE, na.strings = "?", fill = TRUE, row.names = 2)
cigs[, 1] = NULL;

plot(cigs)

# check component
pcs <- prcomp(cigs, scale = T)
pcInf <- summary(pcs)
summary(pcs)
# plot component
biplot(pcs)

eqscplot()
plot(pcInf$importance[3, ], type="b", lty = 1, pch = 19, xlab = "# of PCs", ylab = "Cumulative Proportion Variance Explained")
eqscplot()
plot(pcInf$importance[2, ], type = "b", pch = 19, xlab = "PC#", ylab = "Proportion Variance Explained")
eqscplot()
biplot(prcomp(cigs[-3], scale = T))


