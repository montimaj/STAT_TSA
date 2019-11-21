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
# Stationarity Check
adf.test(fle.data)

fle.fit = lm(fle.data ~ time(fle.data))
fle.residuals = rstudent(fle.fit)
summary(fle.fit)
fle.model = ts(fle.fit$fitted.values, frequency=1, start=1816, end=2019)

plot(fle.data, main='TS Plot: Life Expectency Data')
lines(fle.model, col='red')
legend(1850, 70, legend=c("Actual", "Fitted"), col=c("Black", "Red"), lty=1:1, cex=0.7)