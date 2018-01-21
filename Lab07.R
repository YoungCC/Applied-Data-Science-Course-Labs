rm(list = ls())

#========================================================================
# Author : Yang Cao
# Date: November 16, 2017
# Purpose: Demonstrate Unsupervised Learning and Dimensionality Reduction
#========================================================================

# PCA
attach(USArrests)
View(USArrests)
names(USArrests)
?apply
apply(USArrests, 2, mean)
apply(USArrests, 2, sd)

pr.out <- prcomp(USArrests, scale = TRUE)

names(pr.out)
print(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation 
pr.out$sdev
head(pr.out$x)
biplot(pr.out, scale=0)
pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1),
     type="b")

# K-Means Clustering
set.seed(2)
x <- matrix(rnorm(50*2), ncol=2)
x
plot(x)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
plot(x)

km2.out <- kmeans(x, 2, nstart = 20)
names(km2.out)
print(km2.out)
km2.out$cluster
plot(x, col =(km2.out$cluster + 1),
     main ="K-Means Clustering Results with K = 2",
     xlab="", ylab="", pch=20 , cex=2)

# Hierarchical Clustering
hc.complete <- hclust(dist(x), method= "complete")
hc.average <- hclust(dist(x), method= "average")
hc.single <- hclust(dist(x), method= "single")
par(mfrow = c(1, 3))
plot(hc.complete, main="Complete Linkage", xlab="", sub ="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub ="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub ="", cex=.9)
cutree(hc.complete, 2)
par(mfrow = c(1, 1))
plot(hc.complete)
