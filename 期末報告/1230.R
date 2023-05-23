rm(list = ls(all = TRUE))
graphics.off()

library("readxl")
weather <- read_excel("C:/Users/user/Desktop/多變量11101/期末報告/台灣電力公司_各縣市住宅、服務業及機關用電統計資料 (1).xlsx", sheet = 2)
power <- read_excel("C:/Users/user/Desktop/多變量11101/期末報告/台灣電力公司_各縣市住宅、服務業及機關用電統計資料 (1).xlsx", sheet = 3)
colnames(weather) <- c("date","city","溫度","降雨量","相對溼度")
power <- power[,-7]
colnames(power) <- c("date","city","住宅","服務業","農林漁牧","工業")
x <- merge(weather,power,by=c("date","city"))
x <- x[order(x$date),]

library(dplyr)
x1 <- x %>% filter(date %in% c("2021年01月","2021年02月","2021年03月","2021年04月",
                               "2021年05月","2021年06月","2021年07月","2021年08月",
                               "2021年09月","2021年10月","2021年11月","2021年12月"))
x1 <- x1 %>% arrange(city)

# pca
# determine the nb of factors
xm <- scale(x1[3:9])
xm <- cbind(x1[1:2],xm)
n1 <- nrow(xm)
eig <- eigen((n1 - 1) * cov(xm[3:9])/n1)
e <- eig$values
plot(e, xlab = "Index", ylab = "Lambda", main = "Eigenvalues") 
abline(h=1, col="blue")
v <- eig$vectors
perc = e/sum(e)
cum  = cumsum(e)/sum(e)
xv   = as.matrix(xm[3:9]) %*% v
xv   = xv * (-1)
corr = cor(xm[3:9], xv)[, 1:3]
r12  = corr[1:7, 1:2]
r13  = cbind(corr[1:7, 1], corr[1:7, 3])
r32  = cbind(corr[1:7, 3], corr[1:7, 2])

# plot of cor of PC1&2
par(mfcol = c(1, 3))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Second PC", 
     main = "PC1 vs PC2", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7")
text(r12, label)
# plot of cor of PC3&2
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "Third PC", ylab = "Second PC", 
     main = "PC2 vs PC3", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7")
text(r32, label)
# plot of cor of PC1&3
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "First PC", ylab = "Third PC", 
     main = "PC1 vs PC3", cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.6, lwd = 2)
abline(h = 0, v = 0)
label = c("X1", "X2", "X3", "X4", "X5", "X6", "X7")
text(r13, label)

x1$district=NULL
for (i in c(1:nrow(x1))){
  if (x1$city[i] == "台中市") {
    x1$district[i]=2
  } else if (x1$city[i] == "台北市") {
    x1$district[i]=1
  } else if (x1$city[i] == "台東市") {
    x1$district[i]=4
  } else if (x1$city[i] == "台南市") {
    x1$district[i]=3
  } else if (x1$city[i] == "宜蘭縣") {
    x1$district[i]=1
  } else if (x1$city[i] == "花蓮縣") {
    x1$district[i]=4
  } else if (x1$city[i] == "金門縣") {
    x1$district[i]=5
  } else if (x1$city[i] == "南投縣") {
    x1$district[i]=2
  } else if (x1$city[i] == "屏東縣") {
    x1$district[i]=3
  } else if (x1$city[i] == "苗栗縣") {
    x1$district[i]=2
  } else if (x1$city[i] == "桃園市") {
    x1$district[i]=1
  } else if (x1$city[i] == "高雄市") {
    x1$district[i]=3
  } else if (x1$city[i] == "基隆市") {
    x1$district[i]=1
  } else if (x1$city[i] == "連江縣") {
    x1$district[i]=5
  } else if (x1$city[i] == "雲林縣") {
    x1$district[i]=2
  } else if (x1$city[i] == "新北市") {
    x1$district[i]=1
  } else if (x1$city[i] == "新竹市") {
    x1$district[i]=1
  } else if (x1$city[i] == "新竹縣") {
    x1$district[i]=1
  } else if (x1$city[i] == "嘉義市") {
    x1$district[i]=3
  } else if (x1$city[i] == "嘉義縣") {
    x1$district[i]=3
  } else if (x1$city[i] == "彰化縣") {
    x1$district[i]=2
  } else if (x1$city[i] == "澎湖縣") {
    x1$district[i]=3
  } 
}

# plot of PC1&2
par(mfcol = c(1, 3))
plot(xv[, 1], xv[, 2], pch = x1$district, col = x1$district, 
     xlab = "PC1", ylab = "PC2", main = "PC1 vs PC2", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)
# plot of PC2&3
plot(xv[, 2], xv[, 3], pch = x1$district, col = x1$district, 
     xlab = "PC2", ylab = "PC3", main = "PC2 vs PC3", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)
# plot of PC1&3
plot(xv[, 1], xv[, 3], pch = x1$district, col = x1$district, 
     xlab = "PC1", ylab = "PC3", main = "PC1 vs PC3", cex.lab = 1.2, 
     cex.axis = 1.2, cex.main = 1.8)

# c <- cancor(xm[3:5],xm[6:9])
# str(c)
# c$cor
# cc1_x1 <- as.matrix(xm[3:5]) %*% c$xcoef[,1]
# cc1_x2 <- as.matrix(xm[6:9]) %*% c$ycoef[,1]
# cor(cc1_x1,cc1_x2)
# cc2_x1 <- as.matrix(xm[3:5]) %*% c$xcoef[,2]
# cc2_x2 <- as.matrix(xm[6:9]) %*% c$ycoef[,2]
# cca_df <- xm[3:9] %>% 
#   mutate(cc1_x1=cc1_x1,
#          cc1_x2=cc1_x2,
#          cc2_x1=cc2_x1,
#          cc2_x2=cc2_x2)
# library(ggplot2)
# cca_df %>% 
#   ggplot(aes(x=cc1_x1,y=cc1_x2))+
#   geom_point()
# cca_df %>% 
#   ggplot(aes(x=cc1_x1,y=cc1_x2, color=xm$相對溼度))+
#   geom_point()
# cca_df %>% 
#   ggplot(aes(x=cc2_x1,y=cc2_x2, color=xm$city))+
#   geom_point()

# cca
# reordering the columns of the matrix
s       = cov(xm[3:9])
sa      = s[1:3, 1:3]
sb      = s[4:7, 4:7]
eiga    = eigen(sa)
eigb    = eigen(sb)
sa2     = eiga$vectors %*% diag(1/sqrt(eiga$values)) %*% t(eiga$vectors)
sb2     = eigb$vectors %*% diag(1/sqrt(eigb$values)) %*% t(eigb$vectors)
k       = sa2 %*% s[1:3, 4:7] %*% sb2
si      = svd(k)
a       = sa2 %*% si$u
b       = sb2 %*% si$v
eta     = as.matrix(xm[, 3:5]) %*% a[, 1]
phi     = as.matrix(xm[, 6:9]) %*% b[, 1]
etaphi  = cbind(eta, phi)
# plot
plot(etaphi, type = "n", xlab = "U1", ylab = "V1", main = "First Canonical Variables")
text(etaphi)
library(ggplot2)
ggplot(as.data.frame(etaphi),aes(x=eta,y=phi, color=xm$city))+ 
  geom_point()+ 
  labs(y = "V1", x = "U1", title = "First Canonical Variables")

# # install.packages("CCA")
# require(CCA)
# cc1 <- cc(xm[,3:5],xm[,6:9])
# # display the canonical correlations
# cc1$cor
# # raw canonical coefficients
# cc1[3:4]
# # compute canonical loadings
# cc2 <- comput(xm[,3:5], xm[,6:9], cc1)
# # display canonical loadings
# cc2[3:6]
# # tests of canonical dimensions
# rho <- cc1$cor
# ## Define number of observations, number of variables in first set, and number of variables in the second set.
# n <- dim(xm[,3:5])[1]
# p <- length(xm[,3:5])
# q <- length(xm[,6:9])
# ## Calculate p-values using the F-approximations of different test statistics:
# install.packages("CCP")
# library(CCP)
# p.asym(si$d, n, p, q, tstat = "Wilks")

