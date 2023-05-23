# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# load data
Cereal <- read.csv("C:/Users/user/Documents/Cereal.csv")

summary(Cereal[1:17,3:10])

# S1.1 Plot box plot
boxplot(Cereal$Calories~Cereal$Manufacturer, xlab = "Manufacturer", ylab = "Values", main = "Calories")
boxplot(Cereal$Protein~Cereal$Manufacturer, xlab = "Manufacturer", ylab = "Values", main = "Protein")
boxplot(Cereal$Fat~Cereal$Manufacturer, xlab = "Manufacturer", ylab = "Values", main = "Fat")
boxplot(Cereal$Sodium~Cereal$Manufacturer, xlab = "Manufacturer", ylab = "Values", main = "Sodium")
boxplot(Cereal$Fiber~Cereal$Manufacturer, xlab = "Manufacturer", ylab = "Values", main = "Fiber")
boxplot(Cereal$Carbohydrates~Cereal$Manufacturer, xlab = "Manufacturer", ylab = "Values", main = "Carbohydrates")
boxplot(Cereal$Sugar~Cereal$Manufacturer, xlab = "Manufacturer", ylab = "Values", main = "Sugar")
boxplot(Cereal$Potassium~Cereal$Manufacturer, xlab = "Manufacturer", ylab = "Values", main = "Potassium")

# S1.2 Histograms
install.packages("lattice")
library(lattice)

G <- subset(Cereal, Cereal$Manufacturer == "G", select = Manufacturer:Potassium)
K <- subset(Cereal, Cereal$Manufacturer == "K", select = Manufacturer:Potassium)
Q <- subset(Cereal, Cereal$Manufacturer == "Q", select = Manufacturer:Potassium)
par(mfrow = c(1, 3))

# Calories
hist(G$Calories,freq = FALSE,xlim = c(40,160),ylim = c(0,0.15),main = "Calories")
hist(K$Calories,freq = FALSE,xlim = c(40,160),ylim = c(0,0.15),main = "Calories")
hist(Q$Calories,freq = FALSE,xlim = c(40,160),ylim = c(0,0.15),main = "Calories")
# Protein
hist(G$Protein,freq = FALSE,xlim = c(1,6),ylim = c(0,0.7),main = "Protein")
hist(K$Protein,freq = FALSE,xlim = c(1,6),ylim = c(0,0.7),main = "Protein")
hist(Q$Protein,freq = FALSE,xlim = c(1,6),ylim = c(0,0.7),main = "Protein")
# Fat
hist(G$Fat,freq = FALSE,xlim = c(0,3),ylim = c(0,4),main = "Fat")
hist(K$Fat,freq = FALSE,xlim = c(0,3),ylim = c(0,4),main = "Fat")
hist(Q$Fat,freq = FALSE,xlim = c(0,3),ylim = c(0,4),main = "Fat")
# Sodium
hist(G$Sodium,freq = FALSE,xlim = c(0,350),ylim = c(0,0.015),main = "Sodium")
hist(K$Sodium,freq = FALSE,xlim = c(0,350),ylim = c(0,0.015),main = "Sodium")
hist(Q$Sodium,freq = FALSE,xlim = c(0,350),ylim = c(0,0.015),main = "Sodium")
# Fiber
hist(G$Fiber,freq = FALSE,xlim = c(0,10),ylim = c(0,0.9),main = "Fiber")
hist(K$Fiber,freq = FALSE,xlim = c(0,10),ylim = c(0,0.9),main = "Fiber")
hist(Q$Fiber,freq = FALSE,xlim = c(0,10),ylim = c(0,0.9),main = "Fiber")
# Carbohydrates
hist(G$Carbohydrates,freq = FALSE,xlim = c(0,25),ylim = c(0,0.2),main = "Carbohydrates")
hist(K$Carbohydrates,freq = FALSE,xlim = c(0,25),ylim = c(0,0.2),main = "Carbohydrates")
hist(Q$Carbohydrates,freq = FALSE,xlim = c(0,25),ylim = c(0,0.2),main = "Carbohydrates")
# Sugar
hist(G$Sugar,freq = FALSE,xlim = c(0,20),ylim = c(0,0.15),main = "Sugar")
hist(K$Sugar,freq = FALSE,xlim = c(0,20),ylim = c(0,0.15),main = "Sugar")
hist(Q$Sugar,freq = FALSE,xlim = c(0,20),ylim = c(0,0.15),main = "Sugar")
# Potassium
hist(G$Potassium,freq = FALSE,xlim = c(0,350),ylim = c(0,0.02),main = "Potassium")
hist(K$Potassium,freq = FALSE,xlim = c(0,350),ylim = c(0,0.02),main = "Potassium")
hist(Q$Potassium,freq = FALSE,xlim = c(0,350),ylim = c(0,0.02),main = "Potassium")

# S2.1 
# Scatterplot Matrix

splom(Cereal[, 3:10], groups=Cereal$Manufacturer, data=Cereal)

# install and load packages
libraries = c("aplpack")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
x <- Cereal[,3:10]
ncolors = 20

# face plot
faces(x, nrow = 4, face.type = 1, scale = TRUE, col.nose = rainbow(ncolors), col.eyes = rainbow(ncolors, 
                                                                                                 start = 0.6, end = 0.85), col.hair = terrain.colors(ncolors), col.face = heat.colors(ncolors), 
      col.lips = rainbow(ncolors, start = 0, end = 1), col.ears = rainbow(ncolors, 
                                                                          start = 0, end = 0.8), plot.faces = TRUE)
