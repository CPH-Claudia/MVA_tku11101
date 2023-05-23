# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("KernSmooth", "graphics")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
xx = read.table("C:/Users/user/Desktop/多變量11101/MVA-ToDo-master/QID-1207-MVAdenbank3/bank2.dat")

# Compute a kernel density estimates
dj = bkde2D(xx[, 4:5], bandwidth = 1.06 * c(sd(xx[, 4]), sd(xx[, 5])) * 200^(-1/5))
d1 = bkde(xx[, 4], gridsize = 51)
d2 = bkde(xx[, 5], gridsize = 51)
dp = (d1$y) %*% t(d2$y)

# plot
persp(d1$x, d2$x, dp, box = FALSE, main = "Joint estimate")
persp(dj$x1, dj$x2, dj$fhat, box = FALSE, main = "Product of estimates")
