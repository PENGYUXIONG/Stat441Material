Cereal.data = read.table("cerealbrands.data", header = TRUE, na.strings = "?", fill = TRUE)
Cereal <- as.matrix(Cereal.data)
library(MASS)

# convert the origin datasets into dist
cerealDist <- dist(Cereal)

# max dimension is 14
i = 1
while (i <= 14){
  output = isoMDS(cerealDist, k = i)
  # make sure there is less than 1% stress
  if (output$stress > 1){
    i <- i + 1
  } else{
    print(paste0("selected dimension: ", i))
    break
  }
}

# the selected dimension is 4d in this case

# plot cmd scaling
data <- cmdscale(dist(Cereal), k = i, eig = T)
plot(data$points, type = 'n', xlab = "pc1", ylab = "pc2")
text(data$points, labels = row.names(Cereal.data), cex = 0.5)

# transpose
# Pre processing
Cereal.slice = (Cereal.data[, 3:14])
Cereal.cor <- cor(Cereal.slice)

Cereal.t.dist <- dist(cor(Cereal.cor))
Cereal.t.MDS <- cmdscale(Cereal.t.dist, k = 2, eig = T)
plot(Cereal.t.MDS$points, type = 'n', xlab = "pc1", ylab = "pc2")
text(Cereal.t.MDS$points, labels = names(Cereal.data), cex = 0.5)

eqscplot()
plot(data$points[, 1], xlab = "x", ylab = "PC1 Scaling")
eqscplot()
plot(data$points[, 2], xlab = "x", ylab = "PC2 Scaling")
eqscplot()
plot(data$points[, 3], xlab = "x", ylab = "PC3 Scaling")
eqscplot()
plot(data$points[, 4], xlab = "x", ylab = "PC4 Scaling")

# MDS scaling
eqscplot()
plot(output$points[, 1], xlab = "x", ylab = "PC1 Scaling")
eqscplot()
plot(output$points[, 2], xlab = "x", ylab = "PC2 Scaling")
eqscplot()
plot(output$points[, 3], xlab = "x", ylab = "PC3 Scaling")
eqscplot()
plot(output$points[, 4], xlab = "x", ylab = "PC4 Scaling")

# biplot
eqscplot()
biplot(prcomp(output$points, scale = T))
