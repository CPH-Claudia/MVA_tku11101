%% clear all variables
clear
close all
clc

%% load data
x   = load('pullover.dat');  
X1  = x(:,1);
X2  = x(:,2);
X   = [ones(length(X2),1) X2];  % matrix with only ones in the first column and X2(price) on the second column 
[b] = regress(X1,X);            % regression of (X1) sales on (X2) price

Y = b(1)+b(2).*X2;

%% plot 
hold on

scatter(X2,X1,75,'k')
plot(X2,Y,'-r','LineWidth',1.5)
title('Pullovers Data')
xlabel('Price (X2)')
ylabel('Sales (X1)')
xlim([78 127])

hold off