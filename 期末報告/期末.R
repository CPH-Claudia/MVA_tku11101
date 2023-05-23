rm(list = ls(all = TRUE))
graphics.off()
#讀檔合併資料----
library("readxl")
library("ggplot2")
library('dplyr')
weather <- read_excel("C:/Users/Wendy0418/Desktop/碩一/應用多變分析/台灣電力公司_各縣市住宅、服務業及機關用電統計資料.xlsx", sheet = 1)
power <- read_excel("C:/Users/Wendy0418/Desktop/碩一/應用多變分析/台灣電力公司_各縣市住宅、服務業及機關用電統計資料.xlsx", sheet = 2)
colnames(weather) <- c("date","city","溫度","降雨量","相對溼度")
power <- power[,-7]
colnames(power) <- c("date","city","住宅","服務業","農林漁牧","工業")
x <- merge(weather,power,by=c("date","city"))
x1 <- x %>% filter(date %in% c("2021年01月","2021年02月","2021年03月","2021年04月",
                               "2021年05月","2021年06月","2021年07月","2021年08月",
                               "2021年09月","2021年10月","2021年11月","2021年12月"))
x1 <- x1 %>% arrange(city)
#不要科學記號
options(scipen = 999)
#敘述統計----
summary(x1[3:9])
sd(x1$溫度);sd(x1$降雨量);sd(x1$相對溼度);
sd(x1$住宅);sd(x1$服務業);sd(x1$農林漁牧);sd(x1$工業)
hist(x1$溫度,xlab = "溫度", main = "溫度直方圖")
hist(x1$降雨量,xlab = "降雨量", main = "降雨量直方圖")
hist(x1$相對溼度,xlab = "相對溼度",  main = "相對溼度直方圖")
hist(x1$住宅,xlab = "住宅",  main = "住宅用電量直方圖")
hist(x1$服務業,xlab = "服務業",  main = "服務業用電量直方圖")
hist(x1$農林漁牧,xlab = "農林漁牧", main = "農林漁牧用電量直方圖")
hist(x1$工業,xlab = "工業",  main = "工業用電量直方圖")

# x1$month=NULL
# for (i in c(1:nrow(x1))){
#   if (x1$date[i] == "2021年01月") {
#     x1$month[i]=1
#   } else if (x1$date[i] ==  "2021年02月") {
#     x1$month[i]=2
#   } else if (x1$date[i] ==  "2021年03月") {
#     x1$month[i]=3
#   } else if (x1$date[i] ==  "2021年04月") {
#     x1$month[i]=4
#   } else if (x1$date[i] ==  "2021年05月") {
#     x1$month[i]=5
#   } else if (x1$date[i] ==  "2021年06月") {
#     x1$month[i]=6
#   } else if (x1$date[i] ==  "2021年07月") {
#     x1$month[i]=7
#   } else if (x1$date[i] ==  "2021年08月") {
#     x1$month[i]=8
#   } else if (x1$date[i] ==  "2021年09月") {
#     x1$month[i]=9
#   } else if (x1$date[i] ==  "2021年10月") {
#     x1$month[i]=10
#   } else if (x1$date[i] ==  "2021年11月") {
#     x1$month[i]=11
#   } else {
#     x1$month[i]=12
#   }
# }
xm <- scale(x1[3:9])
xm <- cbind(x1[1:2],xm)

#EDA----
#擷取資料
x21 <- xm %>% filter(city %in% c("台中市")) 
x22 <- xm %>% filter(city %in% c("台北市"))
x23 <- xm %>% filter(city %in% c("台東縣"))
x24 <- xm %>% filter(city %in% c("台南市"))
x25 <- xm %>% filter(city %in% c("宜蘭縣"))
x26 <- xm %>% filter(city %in% c("花蓮縣"))
x27 <- xm %>% filter(city %in% c("金門縣"))
x28 <- xm %>% filter(city %in% c("南投縣"))
x29 <- xm %>% filter(city %in% c("屏東縣"))
x210 <- xm %>% filter(city %in% c("苗栗縣"))
x211 <- xm %>% filter(city %in% c("桃園市"))
x212 <- xm %>% filter(city %in% c("高雄市"))
x213 <- xm %>% filter(city %in% c("基隆市"))
x214 <- xm %>% filter(city %in% c("連江縣"))
x215 <- xm %>% filter(city %in% c("雲林縣"))
x216 <- xm %>% filter(city %in% c("新北市"))
x217 <- xm %>% filter(city %in% c("新竹市"))
x218 <- xm %>% filter(city %in% c("新竹縣"))
x219 <- xm %>% filter(city %in% c("嘉義市"))
x220 <- xm %>% filter(city %in% c("嘉義縣"))
x221 <- xm %>% filter(city %in% c("彰化縣"))
x222 <- xm %>% filter(city %in% c("澎湖縣"))
#箱型圖
boxplot(x21[,6:9],main="台中市")
boxplot(x22[,6:9],main="台北市")
boxplot(x23[,6:9],main="台東縣")
boxplot(x24[,6:9],main="台南市")
boxplot(x25[,6:9],main="宜蘭縣")
boxplot(x26[,6:9],main="花蓮縣")
boxplot(x27[,6:9],main="金門縣")
boxplot(x28[,6:9],main="南投縣")
boxplot(x29[,6:9],main="屏東縣")
boxplot(x210[,6:9],main="苗栗縣")
boxplot(x211[,6:9],main="桃園市")
boxplot(x212[,6:9],main="高雄市")
boxplot(x213[,6:9],main="基隆市")
boxplot(x214[,6:9],main="連江縣")
boxplot(x215[,6:9],main="雲林縣")
boxplot(x216[,6:9],main="新北市")
boxplot(x217[,6:9],main="新竹市")
boxplot(x218[,6:9],main="新竹縣")
boxplot(x219[,6:9],main="嘉義市")
boxplot(x220[,6:9],main="嘉義縣")
boxplot(x221[,6:9],main="彰化縣")
boxplot(x222[,6:9],main="澎湖縣")

#散佈矩陣
require(ggplot2)
require(GGally)
ggpairs(xm[3:9])
#cluster----
##陡坡圖----
xm <- scale(x1[3:9])
n1 <- nrow(x1)
eig <- eigen((n1 - 1) * cov(xm)/n1)
e <- eig$values
plot(e, xlab = "Index", ylab = "Lambda", main = "Eigenvalues") 
abline(h=1, col="blue")
#2021年1月----
set.seed(1231)
##kmeans----
library('factoextra')
library('cluster')
x11 <- x %>% filter(date %in% c("2021年01月"))
rownames(x11)=x11[,2]
x11 <- x11[3:9]
ruspini_scaled <- scale(x11)
summary(ruspini_scaled)
km1 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km1$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km1, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x11)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree1 = cutree(w,3)
table(tree1)
cbind(ruspini_scaled,tree1)
t1   = subset(x11, tree1 == 1)
t2   = subset(x11, tree1 == 2)
t3   = subset(x11, tree1 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x11, tree1 == "1")), colMeans(subset(x11, tree1 == "2")), 
            colMeans(subset(x11, tree1 == "3")))
##混淆矩陣 1月 ----
table(tree1,km1$cluster)

#2021年2月----
set.seed(1231)
##kmeans----
x12 <- x %>% filter(date %in% c("2021年02月"))
rownames(x12)=x12[,2]
x12 <- x12[3:9]
ruspini_scaled <- scale(x12)
summary(ruspini_scaled)
km2 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km2$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km2, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x12)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree2 = cutree(w,3)
table(tree2)
cbind(ruspini_scaled,tree2)
t1   = subset(x12, tree2 == 1)
t2   = subset(x12, tree2 == 2)
t3   = subset(x12, tree2 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x12, tree2 == "1")), colMeans(subset(x12, tree2 == "2")), 
            colMeans(subset(x12, tree2 == "3")))
##混淆矩陣 2月----
table(tree2,km2$cluster)

#2021年3月----
set.seed(1231)
##kmeans----
x13 <- x %>% filter(date %in% c("2021年03月"))
rownames(x13)=x13[,2]
x13 <- x13[3:9]
ruspini_scaled <- scale(x13)
summary(ruspini_scaled)
km3 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km3$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km3, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x13)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree3 = cutree(w,3)
table(tree3)
cbind(ruspini_scaled,tree3)
t1   = subset(x13, tree3 == 1)
t2   = subset(x13, tree3 == 2)
t3   = subset(x13, tree3 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x13, tree3 == "1")), colMeans(subset(x13, tree3 == "2")), 
            colMeans(subset(x13, tree3 == "3")))
##混淆矩陣 3月----
table(tree3,km3$cluster)

#2021年4月----
set.seed(1231)
##kmeans----
x14 <- x %>% filter(date %in% c("2021年04月"))
rownames(x14)=x14[,2]
x14 <- x14[3:9]
ruspini_scaled <- scale(x14)
summary(ruspini_scaled)
km4 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km4$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km4, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x14)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree4 = cutree(w,3)
table(tree4)
cbind(ruspini_scaled,tree4)
t1   = subset(x14, tree4 == 1)
t2   = subset(x14, tree4 == 2)
t3   = subset(x14, tree4 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x14, tree4 == "1")), colMeans(subset(x14, tree4 == "2")), 
            colMeans(subset(x14, tree4 == "3")))
##混淆矩陣 4月----
table(tree4,km4$cluster)

#2021年5月----
set.seed(1231)
##kmeans----
x15 <- x %>% filter(date %in% c("2021年05月"))
rownames(x15)=x15[,2]
x15 <- x15[3:9]
ruspini_scaled <- scale(x15)
summary(ruspini_scaled)
km5 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km5$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km5, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x15)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree5 = cutree(w,3)
table(tree5)
cbind(ruspini_scaled,tree5)
t1   = subset(x15, tree5 == 1)
t2   = subset(x15, tree5 == 2)
t3   = subset(x15, tree5 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x15, tree5 == "1")), colMeans(subset(x15, tree5 == "2")), 
            colMeans(subset(x15, tree5 == "3")))
##混淆矩陣 5月----
table(tree5,km5$cluster)

#2021年6月----
set.seed(1231)
##kmeans----
x16 <- x %>% filter(date %in% c("2021年06月"))
rownames(x16)=x16[,2]
x16 <- x16[3:9]
ruspini_scaled <- scale(x16)
summary(ruspini_scaled)
km6 <- kmeans(ruspini_scaled, centers = 3, nstart = 10)
ruspini_clustered <- cbind(ruspini_scaled,km6$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km6, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x16)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree6 = cutree(w,3)
table(tree6)
cbind(ruspini_scaled,tree6)
t1   = subset(x16, tree6 == 1)
t2   = subset(x16, tree6 == 2)
t3   = subset(x16, tree6 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x16, tree6 == "1")), colMeans(subset(x16, tree6 == "2")), 
            colMeans(subset(x16, tree6 == "3")))
##混淆矩陣 6月----
table(tree6,km6$cluster)

#2021年7月----
set.seed(1231)
##kmeans----
x17 <- x %>% filter(date %in% c("2021年07月"))
rownames(x17)=x17[,2]
x17 <- x17[3:9]
ruspini_scaled <- scale(x17)
summary(ruspini_scaled)
km7 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km7$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km7, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x17)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree7 = cutree(w,3)
table(tree7)
cbind(ruspini_scaled,tree7)
t1   = subset(x17, tree7 == 1)
t2   = subset(x17, tree7 == 2)
t3   = subset(x17, tree7 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x17, tree7 == "1")), colMeans(subset(x17, tree7 == "2")), 
            colMeans(subset(x17, tree7 == "3")))
##混淆矩陣 7月----
table(tree7,km7$cluster)

#2021年8月----
set.seed(1231)
##kmeans----
x18 <- x %>% filter(date %in% c("2021年08月"))
rownames(x18)=x18[,2]
x18 <- x18[3:9]
ruspini_scaled <- scale(x18)
summary(ruspini_scaled)
km8 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km8$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km8, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x18)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree8 = cutree(w,3)
table(tree8)
cbind(ruspini_scaled,tree8)
t1   = subset(x18, tree8 == 1)
t2   = subset(x18, tree8 == 2)
t3   = subset(x18, tree8 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x18, tree8 == "1")), colMeans(subset(x18, tree8 == "2")), 
            colMeans(subset(x18, tree8 == "3")))
##混淆矩陣 8月----
table(tree8,km8$cluster)

#2021年9月----
set.seed(1231)
##kmeans----
x19 <- x %>% filter(date %in% c("2021年09月"))
rownames(x19)=x19[,2]
x19 <- x19[3:9]
ruspini_scaled <- scale(x19)
summary(ruspini_scaled)
km9 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km9$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km9, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x19)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree9 = cutree(w,3)
table(tree9)
cbind(ruspini_scaled,tree9)
t1   = subset(x19, tree9 == 1)
t2   = subset(x19, tree9 == 2)
t3   = subset(x19, tree9 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x19, tree9 == "1")), colMeans(subset(x19, tree9 == "2")), 
            colMeans(subset(x19, tree9 == "3")))
##混淆矩陣 9月----
table(tree9,km9$cluster)

#2021年10月----
set.seed(1231)
##kmeans----
x110 <- x %>% filter(date %in% c("2021年10月"))
rownames(x110)=x110[,2]
x110 <- x110[3:9]
ruspini_scaled <- scale(x110)
summary(ruspini_scaled)
km10 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km10$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km10, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x110)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree10 = cutree(w,3)
table(tree10)
cbind(ruspini_scaled,tree10)
t1   = subset(x110, tree10 == 1)
t2   = subset(x110, tree10 == 2)
t3   = subset(x110, tree10 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x110, tree10 == "1")), colMeans(subset(x110, tree10 == "2")), 
            colMeans(subset(x110, tree10 == "3")))
##混淆矩陣 10月----
table(tree10,km10$cluster)

#2021年11月----
set.seed(1231)
##kmeans----
x111 <- x %>% filter(date %in% c("2021年11月"))
rownames(x111)=x111[,2]
x111 <- x111[3:9]
ruspini_scaled <- scale(x111)
summary(ruspini_scaled)
km11 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km11$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km11, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x111)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree11 = cutree(w,3)
table(tree11)
cbind(ruspini_scaled,tree11)
t1   = subset(x111, tree11 == 1)
t2   = subset(x111, tree11 == 2)
t3   = subset(x111, tree11 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x111, tree11 == "1")), colMeans(subset(x111, tree11 == "2")), 
            colMeans(subset(x111, tree11 == "3")))
##混淆矩陣 11月----
table(tree11,km11$cluster)

#2021年12月----
set.seed(1231)
##kmeans----
x112 <- x %>% filter(date %in% c("2021年12月"))
rownames(x112)=x112[,2]
x112 <- x112[3:9]
ruspini_scaled <- scale(x112)
summary(ruspini_scaled)
km12 <- kmeans(ruspini_scaled, centers = 3, nstart = 10);
ruspini_clustered <- cbind(ruspini_scaled,km12$cluster)
ruspini_clustered <- data.frame(ruspini_clustered)
fviz_cluster(km12, data = ruspini_scaled,
             ellipse.type = "euclid",star.plot=T,repel=T,
             geom = c("point","text"),palette='jco',main="",
             ggtheme=theme_minimal())+
  theme(axis.title = element_blank())
##Ward----
w = hclust(dist(x112)^2, method = "ward.D")
fviz_dend(w,k = 3)
#rect.hclust(w, k = 3, border="red")
tree12 = cutree(w,3)
table(tree12)
cbind(ruspini_scaled,tree12)
t1   = subset(x112, tree12 == 1)
t2   = subset(x112, tree12 == 2)
t3   = subset(x112, tree12 == 3)
# means for Cluster 1 、 Cluster 2、Cluster3 and Cluster4
mc  = cbind(colMeans(subset(x112, tree12 == "1")), colMeans(subset(x112, tree12 == "2")), 
            colMeans(subset(x112, tree12 == "3")))
##混淆矩陣 12月----
table(tree12,km12$cluster)
