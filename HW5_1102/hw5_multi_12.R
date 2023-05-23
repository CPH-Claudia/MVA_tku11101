# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
install.packages("dplyr")
install.packages("ggpubr")

# import data
bank <- read.table("C:/Users/user/Desktop/多變量11101/MVA-ToDo-master/QID-948-MVApcabankr/bank2.dat")
x4 <- bank$V4
x5 <- bank$V5
x45 <- as.data.frame(cbind(bank[, 4],bank[, 5]))

# Shapiro-Wilk test
# p-value>0.05:不拒絕虛無假設(符合常態分佈)
# p-value<0.05:拒絕虛無假設(不符合常態分佈)
shapiro.test(x4)
shapiro.test(x5)

# Q-Q plot
library(ggpubr)
ggqqplot(x4,color = 'blue', main = 'x4')
ggqqplot(x5,color = 'red', main = 'x5')

# scatter plot with linear fit line
ggplot(bank,
       aes(x = x4, y = x5)) +
  geom_point(color= "steelblue") +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(y = "Distance of inner frame to theupper border(x5)",
       x = "Distance of inner frame to the lower border(x4)",
       title = "Scatter Plot")

# chi-square plot
# install and load packages
install.packages("mvoutlier")
install.packages("sgeostat")
library(mvoutlier)
# draw the plot
chisq.plot(x45, quan=1/2, ask=TRUE)

# detecting outliers
# z score
install.packages("outliers")
library(outliers)
z.scores <- x45 %>%  scores(type = "z")
# MVN
install.packages("MVN")
library(MVN)
result_uni <- mvn(x45, mvnTest = "mardia", univariateTest = "SW", showOutliers = TRUE)
result_multi <- mvn(x45, mvnTest = "mardia", multivariateOutlierMethod = "quan", showOutliers = TRUE)
result_multi <- mvn(x45, mvnTest = "mardia", multivariateOutlierMethod = "adj", showOutliers = TRUE)
result_uniqq <- mvn(x45, mvnTest = "mardia", univariatePlot = "qqplot")
result_multiqq <- mvn(x45, mvnTest = "mardia", multivariatePlot = "qq")
result_muticon <- mvn(x45, mvnTest = "mardia", multivariatePlot = "contour")
