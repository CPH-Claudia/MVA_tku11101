[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MVAedfnormal** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: MVAedfnormal

Published in: Applied Multivariate Statistical Analysis

Description: Draws n observations from a standard normal distribution and plots its empirical distribution function (edf) vs. the normal cumulative distribution function (cdf). Number of draws can be entered interactively.

Keywords: standard-normal, multivariate, plot, graphical representation, edf, cdf, standard, normal, distribution

See also: MVAedfbootstrap

Author: Vladimir Georgescu, Jorge Patron, Song Song, Awdesch Melzer

Submitted: Wed, March 14 2012 by Dedy Dwi Prastyo
Submitted[Matlab]: Thu, November 24 2016 by Lily Medina

Example: Shows the EDF and CDF for the empirical and theoretical distribution with n=100 and n=1000.

```

![Picture1](MVAedfnormal1_matlab.png)

![Picture2](MVAedfnormal2_matlab.png)

![Picture3](MVAedfnormal_1.png)

![Picture4](MVAedfnormal_2.png)

### MATLAB Code
```matlab

%% clear all variables
close all
clear
clc

%% set input
disp('Please input number of draws n as') ;
disp(' ') ;
n = input('[n]=');

%% generate random variables
y = normrnd(0,1,n,1); %Generate standard normal random numbers

%% plot
cdfplot(y)            %Plot the empirical distribution function

hold on

x = -3:0.1:3;
f = normcdf(x,0,1);   %Generate normal cumulative distribution function

plot(x,f,'r','LineWidth',2.5)
legend('Empirical','Theoretical','Location','NW')
title('EDF and CFD')
xlabel('X')
ylabel('EDF(X), CDF(X)')

hold off
```

automatically created on 2018-05-28

### R Code
```r


# close windows and clear variables
graphics.off()
rm(list = ls(all = TRUE))

# input parameters
print("Please input number of draws n as: 100")
print("then press enter two times")
para = scan()

while (length(para) < 1) {
  print("Not enough input arguments. Please input a scalar like 100")
  print(" ")
  print("[n]=")
  para = scan()
}
n = para[1]

if (n <= 1) {
  print("MVAedfnormal: Number of observations must be larger than 0. Please input again. n=")
  n = scan()
}
set.seed(80)

# Generate standard normal random numbers
y = rnorm(n, 0, 1)  
y = ecdf(y)

# Plot of empirical distribution function
plot(y, pch = NaN, verticals = TRUE, col = "blue3", lwd = 1.5, xlim = c(-3, 
  3), main = "EDF and CFD", ylab = "EDF(X), CDF(X)", xlab = "X")  
lines(seq(-3, 3, 0.1), pnorm(seq(-3, 3, 0.1), 0, 1), col = "red3", lwd = 2.5)
legend(x = -3, y = 1, legend = c("Empirical", "Theoretical"), pch = c(20, 
  20), col = c("blue3", "red3"))

```

automatically created on 2018-05-28