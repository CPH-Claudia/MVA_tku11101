[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MVAdrug** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: MVAdrug

Published in: Applied Multivariate Statistical Analysis

Description: Produces a three-way contingency table for the drug consumption data from Everitt and Dunn.

Keywords: contingency-table, regression, hypothesis-testing, chi-square, test, chi-square test, statistics, sas

See also: MMSTATassociation, MVAdrugLogistic, MVAdrugsim, MVAdrug3waysTab

Author: Zografia Anastasiadou
Author[SAS]: Svetlana Bykovskaya
Author[Matlab]: Lasse Groth

Submitted: Wed, October 08 2014 by Sergey Nasekin
Submitted[SAS]: Tue, April 05 2016 by Svetlana Bykovskaya
Submitted[Matlab]: Thu, December 08 2016 by Piedad Castro

Output: Top table for men and bottom table for women.

```

### MATLAB Code
```matlab

%% clear all variables and console and close windows
clear
clc
close all

%% Specifics of the dataset
Q = 3; %number of variables
I = 2; %sex M - F
J = 2; %drug Yes - No
K = 5; %age category 16-29, 30-44, 45-64, 65-74, 75++

%% Truncated dataset with first observations missing.
zi = [1 0 1 0 1 0 0 0 0  21
      1 0 1 0 0 1 0 0 0  32
      1 0 1 0 0 0 1 0 0  70
      1 0 1 0 0 0 0 1 0  43
      1 0 1 0 0 0 0 0 1  19
      1 0 0 1 1 0 0 0 0 683
      1 0 0 1 0 1 0 0 0 596
      1 0 0 1 0 0 1 0 0 705
      1 0 0 1 0 0 0 1 0 295
      1 0 0 1 0 0 0 0 1  99
      0 1 1 0 1 0 0 0 0  46
      0 1 1 0 0 1 0 0 0  89
      0 1 1 0 0 0 1 0 0 169
      0 1 1 0 0 0 0 1 0  98
      0 1 1 0 0 0 0 0 1  51
   	  0 1 0 1 1 0 0 0 0 738
      0 1 0 1 0 1 0 0 0 700
      0 1 0 1 0 0 1 0 0 847
      0 1 0 1 0 0 0 1 0 336
      0 1 0 1 0 0 0 0 1 196];
  
namind = ['my1' 'my2' 'my3' 'my4' 'my5' 'mn1' 'mn2' 'mn3' 'mn4' 'mn5'
          'fy1' 'fy2' 'fy3' 'fy4' 'fy5' 'fn1' 'fn2' 'fn3' 'fn4' 'fn5'];
      
%% name of columns
namvar = ['M ' 'F ' 'DY' 'DN' 'A1' 'A2' 'A3' 'A4' 'A5'];

%% Burt table
BURT = [2563	0	   185 	2378	704	628	775	338	118
        0	  3270	453	2817	784	789	1016	434	247
        185  453 	638	0	   67 	121   239	141	70
        2378 2817   0     5195  1421  1296  1552  631  295
        704  784    67    1421   1488   0     0     0     0
        628  789    121   1296   0     1417  0     0     0
        775  1016   239   1552   0     0    1791   0     0
        338  434    141   631    0     0     0     772   0
        118  247    70    295    0     0     0     0    365];

%% Read the relevant data
men   = [zi(1:5,10),zi(6:10,10)];
men   = vertcat([1,0],men);
women = [zi(11:15,10),zi(16:20,10)];
women = vertcat([1,0],women);

%% Display the table
disp('Men')
disp('    Y/N   A1    A2    A3    A4    A5')
disp(men')
disp('Women')
disp('    Y/N   A1    A2    A3    A4    A5')
disp(women')
```

automatically created on 2018-05-28

### R Code
```r


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Specifics of the dataset
Q = 3  # number of variables
I = 2  # sex M - F
J = 2  # drug Yes - No
K = 5  # age category 16-29, 30-44, 45-64, 65-74, 75++

# Truncated dataset with first observations missing
zi = rbind(c(1, 0, 1, 0, 1, 0, 0, 0, 0, 21), c(1, 0, 1, 0, 0, 1, 0, 0, 0, 32), c(1, 
    0, 1, 0, 0, 0, 1, 0, 0, 70), c(1, 0, 1, 0, 0, 0, 0, 1, 0, 43), c(1, 0, 1, 0, 
    0, 0, 0, 0, 1, 19), c(1, 0, 0, 1, 1, 0, 0, 0, 0, 683), c(1, 0, 0, 1, 0, 1, 0, 
    0, 0, 596), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 705), c(1, 0, 0, 1, 0, 0, 0, 1, 0, 295), 
    c(1, 0, 0, 1, 0, 0, 0, 0, 1, 99), c(0, 1, 1, 0, 1, 0, 0, 0, 0, 46), c(0, 1, 1, 
        0, 0, 1, 0, 0, 0, 89), c(0, 1, 1, 0, 0, 0, 1, 0, 0, 169), c(0, 1, 1, 0, 0, 
        0, 0, 1, 0, 98), c(0, 1, 1, 0, 0, 0, 0, 0, 1, 51), c(0, 1, 0, 1, 1, 0, 0, 
        0, 0, 738), c(0, 1, 0, 1, 0, 1, 0, 0, 0, 700), c(0, 1, 0, 1, 0, 0, 1, 0, 
        0, 847), c(0, 1, 0, 1, 0, 0, 0, 1, 0, 336), c(0, 1, 0, 1, 0, 0, 0, 0, 1, 
        196))
# namind = c('my1','my2','my3','my4','my5','mn1','mn2','mn3','mn4','mn5','fy1','fy2','fy3','fy4','fy5','fn1','fn2','fn3','fn4','fn5')
# name of columns namvar = ('M','F','DY','DN','A1','A2','A3','A4','A5') 

# Read the relevant data
men     = cbind(zi[1:5, 10], zi[6:10, 10])
women   = cbind(zi[11:15, 10], zi[16:20, 10])
a       = c(men[, 1], men[, 2], women[, 1], women[, 2])
age     = rep(c("A1", "A2", "A3", "A4", "A5"), 4)
drug    = rep(c("DY", "DY", "DY", "DY", "DY", "DN", "DN", "DN", "DN", "DN"), 2)
gender  = c(rep("men", 10), rep("women", 10))
xtabs(a ~ drug + age + gender)

```

automatically created on 2018-05-28

### SAS Code
```sas

proc iml;
  * Specifics of the dataset;
  Q = 3;  * number of variables;
  I = 2;  * sex M - F;
  J = 2;  * drug Yes - No;
  K = 5;  * age category 16-29, 30-44, 45-64, 65-74, 75++;
  
  * Truncated dataset with first observations missing;
  zi = ({1, 0, 1, 0, 1, 0, 0, 0, 0, 21} ||
    {1, 0, 1, 0, 0, 1, 0, 0, 0, 32 } ||
    {1, 0, 1, 0, 0, 0, 1, 0, 0, 70 } || 
    {1, 0, 1, 0, 0, 0, 0, 1, 0, 43 } ||
    {1, 0, 1, 0, 0, 0, 0, 0, 1, 19 } || 
    {1, 0, 0, 1, 1, 0, 0, 0, 0, 683} ||
    {1, 0, 0, 1, 0, 1, 0, 0, 0, 596} ||
    {1, 0, 0, 1, 0, 0, 1, 0, 0, 705} ||
    {1, 0, 0, 1, 0, 0, 0, 1, 0, 295} ||
    {1, 0, 0, 1, 0, 0, 0, 0, 1, 99 } || 
    {0, 1, 1, 0, 1, 0, 0, 0, 0, 46 } || 
    {0, 1, 1, 0, 0, 1, 0, 0, 0, 89 } || 
    {0, 1, 1, 0, 0, 0, 1, 0, 0, 169} ||
    {0, 1, 1, 0, 0, 0, 0, 1, 0, 98 } || 
    {0, 1, 1, 0, 0, 0, 0, 0, 1, 51 } || 
    {0, 1, 0, 1, 1, 0, 0, 0, 0, 738} || 
    {0, 1, 0, 1, 0, 1, 0, 0, 0, 700} || 
    {0, 1, 0, 1, 0, 0, 1, 0, 0, 847} ||
    {0, 1, 0, 1, 0, 0, 0, 1, 0, 336} || 
    {0, 1, 0, 1, 0, 0, 0, 0, 1, 196})`;
    
  * namind = {'my1','my2','my3','my4','my5',
    'mn1','mn2','mn3','mn4','mn5',
    'fy1','fy2','fy3','fy4','fy5',
    'fn1','fn2','fn3','fn4','fn5'};
  * namvar = {'M','F','DY','DN','A1','A2','A3','A4','A5'}; * name of columns;
  
  * Read the relevant data;
  men    = zi[1:5, 10] || zi[6:10, 10];
  women  = zi[11:15, 10] || zi[16:20, 10];
  a      = (men[, 1] // men[, 2] // women[, 1] // women[, 2])`;
  age    = repeat({'A1' 'A2' 'A3' 'A4' 'A5'}, 1, 4);
  t      = repeat({'DY'}, 1, 5) || repeat({'DN'}, 1, 5);
  drug   = t || t;
  mgen   = repeat({'men'}, 1, 10);
  wgen   = repeat({'women'}, 1, 10);
  gender = mgen || wgen;
	
  create gendertable var {"a" "drug" "age" "gender"};
    append;
  close gendertable;
quit;
  
proc freq data = gendertable;
  tables gender*drug*age / nocol norow nopercent;
  weight a;
run;
  


```

automatically created on 2018-05-28