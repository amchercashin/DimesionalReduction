set.seed(12345)

par(mar = rep(02, 4))
dataMatrix <-matrix(rnorm(400), nrow = 40)

#Look at data, it is random
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix)

#Flip a coin to make some pattern
set.seed(678910)
for (i in 1:40) {
        coinFlip <- rbinom(1, size = 1, prob = 0.5)
        if (coinFlip) {
                dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each = 5)
              
        }
}

#Look at data again
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix)

#Reoder original data according computed clusterized order of distances between "points"(rows) 
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
#Plot data
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):10])
plot(rowMeans(dataMatrixOrdered), 40:1, xlab="Row mean", ylab="Row", pch=19)
plot(colMeans(dataMatrixOrdered), xlab="Column", ylab="Column mean", pch=19)

#So to SVD, show u and v components - similar to the previous picture
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):10])
plot(svd1$u[,1], 40:1, xlab="Row", ylab="First left singular vector", pch=19)
plot(svd1$v[,1], xlab="Row", ylab="First right singular vector", pch=19)
#d component that explain variation 
par(mfrow=c(1,2))
plot(svd1$d, xlab="Column", ylab="Singular value", pch=19)
plot(svd1$d^2 / sum(svd1$d^2), xlab="Column", ylab="Prop. of variance explained", pch=19)


