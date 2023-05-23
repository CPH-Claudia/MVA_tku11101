# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# 3.4
# cov
car <- read.table("C:/Users/user/Desktop/多變量11101/MVA-ToDo-master/QID-1532-MVAcareffect/carc.dat")
cov(car$V3,car$V9)
# install and load packages
libraries = c("lattice")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# scat
# load data
M = car[, 3]
W = car[, 9]
C = car[, 14]
# point definition
D = C
D[car[, 14] == 2] = 1
D[car[, 14] == 1] = 8
# color definition
P = C
P[car[, 14] == 3] = 4
P[car[, 14] == 2] = 2
P[car[, 14] == 1] = 1
leg = c(8, 1, 3)
# plot
xyplot(W ~ M, pch = D, col = P, xlab = "Mileage (X2)", ylab = "Weight (X8)", main = "Car Data")

# 3.25
bank <- read.table("C:/Users/user/Desktop/多變量11101/MVA-ToDo-master/QID-948-MVApcabankr/bank2.dat")
S <- cov(bank[101:200,])
S
Avalues <- eigen(S)$values
Avectors <- eigen(S)$vectors
Avalues
Avectors
t(Avectors)
J <- Avectors%*%diag(Avalues)%*%t(Avectors)
J
Q <- t(matrix(Avalues))%*%S%*%matrix(Avalues)
Q

# 3.26
a <- matrix(c(1, 1, 1, 1, 1, 1))
a
Sy <- t(a)%*%S%*%a
Sy