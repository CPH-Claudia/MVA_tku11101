[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MVApcp8** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: MVApcp8

Published in: Applied Multivariate Statistical Analysis

Description: Computes parallel coordinates plot with cubic spline interpolation.

Keywords: pcp, parallel-coordinates-plot, financial, spline, interpolation, data visualization, plot, graphical representation

See also: MVApcphousing, MVApcp1, MVApcp2, MVApcp3, MVApcp4, MVApcp5, MVApcp6, MVApcp7

Author: Ji Cao, Song Song, Vladimir Georgescu, Awdesch Melzer

Submitted: Tue, September 09 2014 by Awdesch Melzer
Submitted[Matlab]: Thu, November 17 2016 by Lily Medina

Input:
- z1 : first data point
- z2 : second data point

```

![Picture1](MVApcp8.png)

![Picture2](MVApcp8_matlab.png)

### MATLAB Code
```matlab

%% clear all variables
clear
close all
clc

%% parameter setting 
z1 = [0,2,3,2];
z2 = [3,2,2,1];
z  = [z1;z2];

xx = 0.99:0.01:4;
x  = 1:4;
zz = spline(x,z,xx);

%% plot
plot(x,z,'o',xx,zz, 'linewidth', 2, 'Color','k')
set(gca,'XTick',1:1:4,'YTick',0:0.5:3, 'xlim', [0.95 4.05], 'ylim', [-0.05 3.05])

title('Parallel Coordinate Plot with Cubic Spline')
xlabel('Coordinate')
ylabel('Coordinate Value')

text(1.5,1.4,'A')
text(1.5,2.5,'B')
```

automatically created on 2018-05-28

### R Code
```r


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# parameter settings
z1  = c(0, 2, 3, 2)
z2  = c(3, 2, 2, 1)
n   = 400

x1  = spline(z1, n = n)
x2  = spline(z2, n = n)

# Plot
plot(x1, lwd = 2, type = "l", ylab = "Coordinate Value", xlab = "Coordinate", frame = TRUE, 
    axes = FALSE, xlim = c(1, 4), ylim = c(0, 3))
lines(x2, lwd = 2, col = "red3")
points(c(1, 2), c(3, 2))
points(c(1, 2), c(0, 2))
points(c(2, 3), c(2, 2))
points(c(2, 3), c(2, 3))
points(c(3, 4), c(3, 2))
points(c(3, 4), c(2, 1))
axis(side = 1, at = seq(1, 4), labels = seq(1, 4))
axis(side = 2, at = seq(0, 3), labels = seq(0, 3))
title("Parallel Coordinates Plot with Cubic Spline") 

```

automatically created on 2018-05-28