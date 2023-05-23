rm(list = ls(all = TRUE))
graphics.off()
# load data
x <-  read.table("D:/淡江_碩/多變量11101/uscrime.dat")
n1 <- nrow(x)
n2 <- ncol(x)

# standardize the data
# x <- scale(x)
x <- (x - matrix(mean(as.matrix(x)), n1, n2, byrow = T))/matrix(sqrt((n1 - 1) *
        apply(x, 2, var)/n1), n1, n2, byrow = T)

# spectral decomposition
eig <- eigen((n1 - 1) * cov(x)/n1)
e <- eig$values
v <- eig$vectors
# eigenvalues and percentage
perc = e/sum(e)
cum  = cumsum(e)/sum(e)
xv   = as.matrix(x) %*% v # principal components
xv   = xv * (-1)

# correlation of the first 3 PC
corr = cor(x, xv)[, 1:3]
r12  = corr[1:11, 1:2]
r13  = cbind(corr[1:11, 1], corr[1:11, 3])
r32  = cbind(corr[1:11, 3], corr[1:11, 2])
r123 = corr[1:11, 1:3]

# plot of cor of PC1&2
par(mfrow = c(2, 2))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Second PC", 
     main = "US Crime", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
text(r12, label)
# plot of cor of PC3&2
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "Third PC", ylab = "Second PC", 
     main = "US Crime", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
text(r32, label)
# plot of cor of PC1&3
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Third PC", 
     main = "US Crime", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
text(r13, label)
# plot of cor of PC1&2&3
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "X", ylab = "Y", cex.lab = 1.2, 
     cex.axis = 1.2, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
text(r123, label) 

# plot of PC1&2
par(mfrow = c(2, 2))
plot(xv[, 1], xv[, 2], pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
     rep("red", 12), rep("green1", 16), rep("purple", 13)), xlab = "PC1", ylab = "PC2", main = "First vs. Second PC", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)
# plot of PC2&3
plot(xv[, 2], xv[, 3], pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
     rep("red", 12), rep("green1", 16), rep("purple", 13)), xlab = "PC2", ylab = "PC3", main = "Second vs. Third PC", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)
# plot of PC1&3
plot(xv[, 1], xv[, 3], pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
     rep("red", 12), rep("green1", 16), rep("purple", 13)), xlab = "PC1", ylab = "PC3", main = "First vs. Third PC", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)
# plot of the eigenvalues
plot(e, ylim = c(0, 6), xlab = "Index", ylab = "Lambda", main = "Eigenvalues", 
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8) 
abline(h=1, col="blue")

# 去除x10,x11
x1 <- x[,-(10:11)]
n3 <- ncol(x1)
x11 <- (x1 - matrix(mean(as.matrix(x1)), n1, n3, byrow = T))/matrix(sqrt((n1 - 1) * 
                                                                         apply(x1, 2, var)/n1), n1, n3, byrow = T)
eig1 <- eigen((n1 - 1) * cov(x11)/n1)
e1 <- eig1$values
v1 <- eig1$vectors
perc1 = e1/sum(e1)
cum1  = cumsum(e1)/sum(e1)
xv1   = as.matrix(x11) %*% v1
xv1   = xv1 * (-1)
corr1 = cor(x11, xv1)[, 1:3]

r12_1  = corr1[1:9, 1:2]
r13_1  = cbind(corr1[1:9, 1], corr1[1:9, 3])
r32_1  = cbind(corr1[1:9, 3], corr1[1:9, 2])
r123_1 = corr1[1:9, 1:3]

par(mfrow = c(2, 2))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Second PC", 
     main = "US Crime", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
text(r12_1, label)
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "Third PC", ylab = "Second PC", 
     main = "US Crime", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
text(r32_1, label)
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Third PC", 
     main = "US Crime", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
text(r13_1, label)
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "X", ylab = "Y", 
     main = "US Crime", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
text(r123_1, label)

par(mfrow = c(2, 2))
plot(xv1[, 1], xv1[, 2], pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
                                                                                         rep("red", 12), rep("green1", 16), rep("purple", 13)), xlab = "PC1", ylab = "PC2", main = "First vs. Second PC", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)
plot(xv1[, 2], xv1[, 3], pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
                                                                                         rep("red", 12), rep("green1", 16), rep("purple", 13)), xlab = "PC2", ylab = "PC3", main = "Second vs. Third PC", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)
plot(xv1[, 1], xv1[, 3], pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
                                                                                         rep("red", 12), rep("green1", 16), rep("purple", 13)), xlab = "PC1", ylab = "PC3", main = "First vs. Third PC", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)
plot(e1, ylim = c(0, 6), xlab = "Index", ylab = "Lambda", main = "Eigenvalues", 
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8) 
abline(h=1, col="blue")

# BONUS:只留X3~X9
x2 <- x[,(3:9)]
n4 <- ncol(x2)
x12 <- (x2 - matrix(mean(as.matrix(x2)), n1, n4, byrow = T))/matrix(sqrt((n1 - 1) * 
                                                                           apply(x2, 2, var)/n1), n1, n4, byrow = T)
eig2 <- eigen((n1 - 1) * cov(x12)/n1)
e2 <- eig2$values
v2 <- eig2$vectors
perc2 = e2/sum(e2)
cum2  = cumsum(e2)/sum(e2)
xv2   = as.matrix(x12) %*% v2
xv2   = xv2 * (-1)

corr2 = cor(x12, xv2)[, 1:2]
r12_2  = corr2[1:7, 1:2]

plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Second PC", 
     main = "US Crime", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X3", "X4", "X5", "X6", "X7", "X8", "X9")
text(r12_2, label)

plot(xv2[, 1], xv2[, 2], pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
                                                                                           rep("red", 12), rep("green1", 16), rep("purple", 13)), xlab = "PC1", ylab = "PC2", main = "First vs. Second PC", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)

plot(e2, ylim = c(0, 6), xlab = "Index", ylab = "Lambda", main = "Eigenvalues", 
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8) 
abline(h=1, col="blue")
