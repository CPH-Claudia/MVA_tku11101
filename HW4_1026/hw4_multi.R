# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# 4.1
# install and load packages
libraries = c("MASS", "mnormt")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# parameter settings
n1   = 200  # number of draws
mu1  = c(3, 2)  # mean vector
sig1 = matrix(c(1, -1.5, -1.5, 4), ncol = 2)  # covariance matrix
# bivariate normal sample
set.seed(1024)
x = mvrnorm(n1, mu1, sig1, 2)
# bivariate normal density
xgrid = seq(from = (mu1[1] - 3 * sqrt(sig1[1, 1])), to = (mu1[1] + 3 * sqrt(sig1[1, 1])), 
            length.out = 200)
ygrid = seq(from = (mu1[2] - 3 * sqrt(sig1[2, 2])), to = (mu1[2] + 3 * sqrt(sig1[2, 2])), 
            length.out = 200)
z     = outer(xgrid, ygrid, FUN = function(xgrid, ygrid) {
  dmnorm(cbind(xgrid, ygrid), mean = mu1, varcov = sig1)
})
# contour ellipses
contour(xgrid, ygrid, z, xlim = range(xgrid), ylim = range(ygrid), nlevels = 10, col = c("blue", 
                                                                                         "black", "yellow", "cyan", "red", "magenta", "green", "blue", "black"), lwd = 3, 
        cex.axis = 1, xlab = "X1", ylab = "X2")
title("Contour Ellipses")
# surface plot
persp(xgrid, ygrid, z, theta=-30, phi=25, expand=0.6, ticktype='detailed', xlab="X1", ylab = "X2" )

# 5.1
# parameter settings
n2   = 200  # number of draws
mu2  = c(2, 2)  # mean vector
sig2 = matrix(c(1, 0, 0, 1), ncol = 2)  # covariance matrix
A <- t(matrix(c(1, 1)))
B <- t(matrix(c(1, -1)))
# bivariate normal sample
set.seed(1024)
x2 = mvrnorm(n2, mu2, sig2, 2)
# 先證AX,BX常態，再做共變異數=0
m1 <- A%*%mu2
m2 <- B%*%mu2
s11 <- A%*%sig2%*%t(A) 
s12 <- A%*%sig2%*%t(B) 
s21 <- B%*%sig2%*%t(A) 
s22 <- B%*%sig2%*%t(B) 
S <- matrix(c(s11, s21, s12, s22), ncol = 2)