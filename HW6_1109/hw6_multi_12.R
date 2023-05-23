# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Load data
x = read.table("C:/Users/user/Desktop/多變量11101/MVA-ToDo-master/QID-1659-MVAsimcidif/uscomp2.dat")
y = data.frame(x)

# Create subsets for Energy and Manufacturing
yE = subset(y, y$V8 == "Energy")
yM = subset(y, y$V8 == "Manufacturing")
yEM = rbind(yE[, 2:7],yM[, 2:7])

# Calculate means of groups
exE = cbind(apply(yE[, 2:7], 2, mean)) # 1:by row;2:by column
exM = cbind(apply(yM[, 2:7], 2, mean)) 
# https://kemushi54.github.io/R-basic/apply_family.html

# Estimating variance of the groups observations within the groups and overall
nE  = length(yE[, 1])
nM  = length(yM[, 1])
n   = nE + nM

# number of groups
p   = length(exE)

sE  = ((nE - 1)/nE) * cov(yE[, 2:7]) # S1
sM  = ((nM - 1)/nM) * cov(yM[, 2:7]) # S2

s     = (nE * sE + nM * sM)/(nE + nM)
sinv  = solve(s)
k     = nE * nM * (n - p - 1)/(p * (n^2))

# Computing the test statistic
(f = k * t(exE - exM) %*% sinv %*% (exE - exM))

# Computing the critical value
(critvalue = qf(1 - 0.05, 6, 18)) 
# ALPHA=0.05 95%CI 右尾 IF f>critvalue 拒絕HO
(critvalue = qf(1 - 0.1, 6, 18)) 

# Computes the simultaneous confidence intervals
deltau  = (exE - exM) + sqrt(qf(1 - 0.05, p, n - p - 1) * (1/k) * diag(s))
deltal  = (exE - exM) - sqrt(qf(1 - 0.05, p, n - p - 1) * (1/k) * diag(s))

(confit = cbind(deltal, deltau)) 

# extrapoints
library(MVN)
result_uni <- mvn(yEM, mvnTest = "mardia", univariateTest = "SW", showOutliers = TRUE)
result_multi <- mvn(yEM, mvnTest = "mardia", multivariateOutlierMethod = "quan", showOutliers = TRUE)
