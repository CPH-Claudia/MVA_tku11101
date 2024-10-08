[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MVAsimcibh** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: MVAsimcibh

Published in: Applied Multivariate Statistical Analysis

Description: Tests the equality of 2 groups of Boston Housing data and computes the F-statistic and the critical value of the test and the simultaneous confidence intervals.

Keywords: F-test, F-statistic, critical-value, confidence-interval, test, hypothesis-testing, financial

See also: MVAlinregbh, MVAdiscbh, MVAdescbh, MVAclusbh, MVAboxbhd, MVAaerbh

Author: Vladimir Georgescu, Song Song, Awdesch Melzer
Author[Matlab]:  Wolfgang Haerdle, Vladimir Georgescu, Song Song

Submitted: Wed, April 04 2012 by Dedy Dwi Prastyo
Submitted[Matlab]: Thu, December 08 by Piedad Castro

Datafile: bostonh.dat

```

### MATLAB Code
```matlab

%% clear all variables and console and close windows
clear
clc
close all

%% load data
x = load('bostonh.dat');

%% Transformations
x(:, 1)  = log(x(:, 1));
x(:, 5)  = log(x(:, 5));
x(:, 8)  = log(x(:, 8));
x(:, 11) = exp(0.4 * x(:, 11))/1000;
x(:, 13) = sqrt(x(:, 13));

v1     = x(x(:, 14) <= median(x(:, 14)), :);
v2     = x(x(:, 14) > median(x(:, 14)), :);
x1     = v1(:, [1, 5, 8, 11, 13]);
x2     = v2(:, [1, 5, 8, 11, 13]);
n1     = length(x1);
n2     = length(x2);
n      = n1 + n2;
[~, p] = size(x1);

%% Estimating the mean and the variance
s1   = ((n1 - 1)/n1)*cov(x1);
s2   = ((n2-1)/n2)*cov(x2);
s    = (n1 * s1 + n2 * s2)/(n1 + n2);
ex1  = mean(x1);
ex2  = mean(x2);
sinv = inv(s);
k    = n1 * n2 * (n - p - 1)/(p * (n^2));

%% Computing the test statistic
f = k * (ex1 - ex2) * sinv * (ex1 - ex2)';
disp('F-statistic')
disp(f)

%% Computing the critical value 
critvalue = finv(0.95, 5, 500);
disp('critical value')
disp(critvalue)

%% Computes the simultaneous confidence intervals
deltau = (ex1 - ex2)' + sqrt(finv(0.95, p, n - p - 1) * (1/k) * diag(s));
deltal = (ex1 - ex2)' - sqrt(finv(0.95, p, n - p - 1) * (1/k) * diag(s));

confint = [deltal, deltau];
disp('Simultaneous confidence intervals')
disp(confint)

```

automatically created on 2018-05-28

### R Code
```r


# сlear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# load data
x = read.table("bostonh.dat")

# Transformations
x[, 1]  = log(x[, 1])
x[, 5]  = log(x[, 5])
x[, 8]  = log(x[, 8])
x[, 11] = exp(0.4 * x[, 11])/1000
x[, 13] = sqrt(x[, 13])

data = data.frame(x)
v1    = subset(x, x[, 14] <= median(x[, 14]))
v2    = subset(x, x[, 14] > median(x[, 14]))
x1    = cbind(v1[, 1], v1[, 5], v1[, 8], v1[, 11], v1[, 13])
x2    = cbind(v2[, 1], v2[, 5], v2[, 8], v2[, 11], v2[, 13])
n1    = length(x1[, 1])
n2    = length(x2[, 1])
n     = n1 + n2
a     = dim(x1)
p     = a[2]

# Estimating the mean and the variance
s1    = ((n1 - 1)/n1) * cov(x1)
s2    = ((n2 - 1)/n2) * cov(x2)
s     = (n1 * s1 + n2 * s2)/(n1 + n2)
ex1   = apply(x1, 2, mean)
ex2   = apply(x2, 2, mean)
sinv  = solve(s)
k     = n1 * n2 * (n - p - 1)/(p * (n^2))

# Computing the test statistic
f = k * t(ex1 - ex2) %*% sinv %*% (ex1 - ex2)
print("F-statistic")
print(f)

# Computing the critical value
critvalue = qf(0.95, 5, 500)
print("critical value")
print(critvalue)

# Computes the simultaneous confidence intervals
deltau = (ex1 - ex2) + sqrt(qf(0.95, p, n - p - 1) * (1/k) * diag(s))
deltal = (ex1 - ex2) - sqrt(qf(0.95, p, n - p - 1) * (1/k) * diag(s))

confint = cbind(deltal, deltau)
print("Simultaneous confidence intervals")
print(confint)

```

automatically created on 2018-05-28