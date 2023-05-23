# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# load data
data <- read.table("C:/Users/user/Desktop/多變量11101/uscrime.dat")
x <- data[,(3:9)]
# x <- cbind(x, rep("en"=x[1:9,], "mw"=x[10:21,], "sou"=x[22:37,], "wes"=x[38:50,]))
x1 <- scale(x)  # standardize variable
# define variable names
colnames(x1) = c("murder", "rape", "robbery", "assault", "burglary", "larcery", "autothieft")

row.s    = apply(x, 1, sum)        # row sums
column.s = apply(x, 2, sum)        # column sums
mat.s    = sum(x)                  # matrix sum
D        = matrix(0, nrow = 50, ncol = 50)
# distance for rows
for (i in 1:(dim(x)[1] - 1)) {
  for (j in (i + 1):dim(x)[1]) {
    for (z in 1:dim(x)[2]) {
      D[i, j] = 1/(column.s[z]/mat.s) * ((x[i, z]/row.s[i]) - (x[j, z]/row.s[j]))^2
    }
  }
}
D

# k-means clustering algorithm
# install.packages("ClusterR")
library(ClusterR)
library(cluster)
library(factoextra)
set.seed(1221)
k.means <- kmeans(x1, 4, nstart = 25)
#plot results of final k-means model
fviz_cluster(k.means, data = x1)
#find means of each cluster
mc.km <- cbind(colMeans(subset(x1, k.means$cluster == "1")), colMeans(subset(x1, k.means$cluster == "2")), 
            colMeans(subset(x1, k.means$cluster == "3")), colMeans(subset(x1, k.means$cluster == "4")))
library(matrixStats)
sc.km <- cbind(colSds(subset(x1, k.means$cluster == "1")[, 1:ncol(x1)]), 
               colSds(subset(x1, k.means$cluster == "2")[, 1:ncol(x1)]), 
               colSds(subset(x1, k.means$cluster == "3")[, 1:ncol(x1)]), 
               colSds(subset(x1, k.means$cluster == "4")[, 1:ncol(x1)]))
tbl.km <- cbind(mc.km[, 1], sc.km[, 1]/sqrt(nrow(subset(x1, k.means$cluster == "1"))), 
            mc.km[, 2], sc.km[, 2]/sqrt(nrow(subset(x1, k.means$cluster == "2"))), 
            mc.km[, 3], sc.km[, 3]/sqrt(nrow(subset(x1, k.means$cluster == "3"))), 
            mc.km[, 4], sc.km[, 4]/sqrt(nrow(subset(x1, k.means$cluster == "4"))))
# 蜘蛛圖
dev.new()
plot(x1,type="n", xlab="",ylab="", main="k-means clustering")
points(k.means$centers[1,1],k.means$centers[1,2], col = "black")
points(k.means$centers[2,1],k.means$centers[2,2], col = "black")
# Plot Lines
k.means$cluster
for (i in 1:50){
  segments(x1[i,1], x1[i,2],
           k.means$centers[k.means$cluster[i], 1],k.means$centers[k.means$cluster[i], 2],lwd=2)
}

segments(k.means$centers[1,1],k.means$centers[1,2],k.means$centers[2,1],k.means$centers[2,2],lwd=2)

points(x1, pch=21, cex=3, bg="white")
text(x1, as.character(1:50),col="red3",cex=1.2)

# Ward algorithm
d <- dist(x1, "euclidean", p = 2)   # euclidean distance matrix(p=power of Minkowski distance)
dd <- d^2
w <- hclust(dd, method = "ward.D")  # cluster analysis with ward algorithm
tree.wd = cutree(w, 4)
# plot results of ward algorithm
fviz_cluster(list(data = x1, cluster = tree.wd))

t1   = subset(x1, tree.wd == 1)
t2   = subset(x1, tree.wd == 2)
t3   = subset(x1, tree.wd == 3)
t4   = subset(x1, tree.wd == 4)

# Plot 1: Dendrogram for the standardized data after Ward 
plot(w, hang = -0.1, labels = FALSE, frame.plot = TRUE, ann = FALSE)
title(main = "Ward Dendrogram", ylab = "distance")
rect.hclust(w,k=4) # k=4的分類線

# means for 4 Clusters
mc.wd <- cbind(colMeans(subset(x1, tree.wd == "1")), colMeans(subset(x1, tree.wd == "2")), 
               colMeans(subset(x1, tree.wd == "3")), colMeans(subset(x1, tree.wd == "4")))
# standard deviations for 4 Clusters
sc.wd <- cbind(colSds(t1[, 1:ncol(x1)]), colSds(t2[, 1:ncol(x1)]), 
               colSds(t3[, 1:ncol(x1)]), colSds(t4[, 1:ncol(x1)]))
# means and standard deviations of the standardized variables for 4 Clusters
tbl.wd = cbind(mc.wd[, 1], sc.wd[, 1]/sqrt(nrow(t1)), mc.wd[, 2], sc.wd[, 2]/sqrt(nrow(t2)),
            mc.wd[, 3], sc.wd[, 3]/sqrt(nrow(t3)), mc.wd[, 4], sc.wd[, 4]/sqrt(nrow(t4)))

# spectral decomposition
eig = eigen(cov(x1))
e = eig$values
v = eig$vectors[, 1:2]
dav = x1 %*% v
# PC選取
cum  = cumsum(e)/sum(e)
corr = cor(x1, -dav)[, 1:2]
plot(e, ylim = c(0, 6), xlab = "Index", ylab = "Lambda", main = "Eigenvalues", 
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8) 
abline(h=1, col="blue")

# 自定義點的形狀&顏色
tree.wd[tree.wd == 1] = 1 
tree.wd[tree.wd == 2] = 2
tree.wd[tree.wd == 3] = 3
tree.wd[tree.wd == 4] = 4
tr = tree.wd
tr[tr == 1] = "red"
tr[tr == 2] = "black"
tr[tr == 3] = "blue"
tr[tr == 4] = "orange"

# Scatterplot for the first two PCs displaying the 4 clusters
dev.new()
plot(dav[, 1], dav[, 2], pch = tree.wd, col = tr, xlab = "PC1", ylab = "PC2", main = "first vs. second PC")

# c.f. 
table <- cbind(x1, tree.wd, k.means$cluster)
colnames(table) <- c("murder", "rape", "robbery", "assault", "burglary", 
                     "larcery", "autothieft", "ward", "kmeans")
# correlation coefficient
r <- round(cor(x1), digits = 3)
library(ggplot2)
library(GGally)
# Scatterplot matrix for variables X1 to X7
ggpairs(data = as.data.frame(x1), alpha = 0.5)
# dev.new()
# ggpairs(data = x, mapping = aes(color = tr), alpha = 0.5)
