# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

library(dplyr)
v = read.csv("C:/Users/user/Desktop/多變量11101/期末報告/2022_01_電力.csv")
summary(v$住宅部門售電量.度.)
summary(v$服務業部門.含包燈..度.)
summary(v$農林漁牧售電量.度.)
summary(v$工業部門售電量.度.)
boxplot(v$住宅部門售電量.度.)
boxplot(v$服務業部門.含包燈..度.)
boxplot(v$農林漁牧售電量.度.)
boxplot(v$工業部門售電量.度.)
