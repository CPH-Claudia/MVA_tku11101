[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MVAboxbank1** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: MVAboxbank1

Published in: Applied Multivariate Statistical Analysis

Description: 'Computes boxplots for the length (X1 variable) of the genuine and forged banknotes from the Swiss bank data.'

Keywords: descriptive, descriptive-statistics, financial, data visualization, boxplot, plot, graphical representation

See also: MVAboxbank6, MVAboxbhd, MVAboxcar, MVAboxcity

Author: Vladimir Georgescu, Jorge Patron, Song Song

Submitted: Tue, September 09 2014 by Awdesch Melzer
Submitted[Matlab]: Mon, November 14 2016 by Lily Medina

Datafiles: bank2.dat

```

![Picture1](MVAboxbank1-1_matlab.png)

![Picture2](MVAboxbank1-1_r.png)

### R Code
```r


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# load data
x  = read.table("bank2.dat")
m1 = mean(x[1:100, 1])
m2 = mean(x[101:200, 1])

# plot
boxplot(x[1:100, 1], x[101:200, 1], axes = FALSE, frame = TRUE)
axis(side = 1, at = seq(1, 2), label = c("GENUINE", "COUNTERFEIT"))
axis(side = 2, at = seq(200, 250, 0.5), label = seq(200, 250, 0.5))
title("Swiss Bank Notes")
lines(c(0.6, 1.4), c(m1, m1), lty = "dotted", lwd = 1.2)
lines(c(1.6, 2.4), c(m2, m2), lty = "dotted", lwd = 1.2)
```

automatically created on 2018-05-28

### MATLAB Code
```matlab

%% clear variables and close windows
close all
clear
clc

%%  load data
x  = load('bank2.dat');
m1 = mean(x(1:100,1));
m2 = mean(x(101:200,1));

%%  plot
hold on
line([0.6 1.4],[m1 m1],'Color','k','LineStyle',':','LineWidth',1.2)
line([1.6 2.4],[m2 m2],'Color','k','LineStyle',':','LineWidth',1.2)
boxplot([x(1:100,1) x(101:200,1)],'Symbol','o','labels',{'GENUINE','COUNTERFEIT'},'widths',0.8)
title('Swiss Bank Notes')
hold off
```

automatically created on 2018-05-28