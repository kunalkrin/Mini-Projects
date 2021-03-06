
### Flowers ###
flower = read.csv("flower.csv", header = F)
str(flower)

flowerMatrix <-as.matrix(flower)
str(flowerMatrix)

flowerVector <-as.vector(flowerMatrix)
str(flowerVector)

distance = dist(flowerVector, method="euclidean")
clusterIntensity = hclust(distance, method = "ward")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k = 3, border = "red")
flowerClusters = cutree(clusterIntensity, k = 3)
tapply(flowerVector, flowerClusters, mean)
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes = F)
image(flowerMatrix, axes = F, col = grey(seq(0,1, length = 256)))

###MRI###

healthy <- read.csv("healthy.csv", header = F)
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes = F, col = grey(seq(0,1, length = 256)))
healthyVector <- as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

#k-means
k=5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

healthyClusters = KMC$cluster
KMC$centers[2]

dim(healthyClusters) = c(nrow(healthyMatrix), ncol (healthyMatrix))
image (healthyClusters, axes = F, col = rainbow(k))

#train on tumor
tumor = read.csv("tumor.csv", header = F)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)
KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = F, col = rainbow(k))
