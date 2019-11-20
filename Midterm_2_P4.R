### STAT 5814 MIDTERM 2/PROBLEM 4
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(snpar)
library(stats)
library(forecast)
library(tseries)
library(fUnitRoots)

data = read.table('Data/ExamData/FLE.txt')
fle.data = ts(data$V2, frequency=1, start=1816, end=2019)
par(mfrow=c(1, 1))
plot(fle.data, main='Time Series Plot of French Life Expectency', xlab='Time (Years)', ylab='Annual Average Life Expectancy (years)')
