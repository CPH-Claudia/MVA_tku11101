# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# load data
data <- read.table("C:/Users/user/Desktop/多變量11101/uscrime.dat")
x <- data[,(3:9)]
# define variable names
colnames(x) = c("murder", "rape", "robbery", "assault", "burglary", "larcery", "autothieft")

# correlation matrix
r = cor(x)

# determine the nb of factors
n1 <- nrow(x)
n2 <- ncol(x)
xm <- (x - matrix(mean(as.matrix(x)), n1, n2, byrow = T))/matrix(sqrt((n1 - 1) * 
                                                                           apply(x, 2, var)/n1), n1, n2, byrow = T)
eig <- eigen((n1 - 1) * cov(xm)/n1)
e <- eig$values
plot(e, ylim = c(0, 6), xlab = "Index", ylab = "Lambda", main = "Eigenvalues", 
     cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.8) 
abline(h=1, col="blue")

# factor analysis
# without rotate
x.fac <- factanal(x, factors = 2, rotation = "none", scores = "regression")
x.fac$loadings[,1]
x.fac$loadings[,2]
x.fac$scores
# rotated
x.fac.r <- factanal(x, factors = 2, rotation="varimax", scores = "regression")
x.fac.r$loadings[,1]
x.fac.r$loadings[,2]
x.fac.r$scores
com <- 1 - x.fac.r$uniquenesses

# residual matrix
Lambda <- x.fac$loadings
Psi <- diag(x.fac$uniquenesses)
S <- x.fac$correlation
Sigma <- Lambda %*% t(Lambda) + Psi
round(S - Sigma, 4) # round the result to 4 digits
det(Sigma)/det(S)

# scatter plot
plot(x.fac$scores, pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
                                                                                       rep("red", 12), rep("green1", 16), rep("purple", 13)))

plot(x.fac.r$scores, pch = c(rep(1, 9), rep(3, 12), rep(11, 16), rep(13, 13)), col = c(rep("blue", 9), 
                                                                                     rep("red", 12), rep("green1", 16), rep("purple", 13)))
par(mfrow = c(1,2))
plot(x.fac$loadings[,1], 
     x.fac$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     main = "No rotation")
abline(h = 0, v = 0)

plot(x.fac.r$loadings[,1], 
     x.fac.r$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     main = "Varimax rotation")

text(x.fac.r$loadings[,1], 
     x.fac.r$loadings[,2]+0.05,
     colnames(x),
     col="blue")
abline(h = 0, v = 0)

# 法二
library(psych)
fa <- fa(r, nfactors = 2, rotate = "none", fm = "ml", scores = "regression") # ml:最大似然法;pa:主軸迭代法;wls:加權最小二乘法
fa.varimax <- fa(r, nfactors = 2, rotate = "varimax", fm = "ml", scores = "regression")
factor.plot(fa.varimax, labels = rownames(fa.varimax$loadings), pch = fa.varimax$loadings)
fa.diagram(fa.varimax, digits = 3)
# digits = 3表示保留3為小數