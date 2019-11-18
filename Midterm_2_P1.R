### STAT 5814 MIDTERM 2/PROBLEM 1
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(snpar)

#get data
data(wages)
par(mfrow = c(1, 1))
plot(wages, type='o', ylab='Wages (USD/hr)', xlab='Time (Months)', main='Monthly Values of the Average Hourly Wages')

#linear model
wages.lm = lm(wages~time(wages))
summary(wages.lm)
model = ts(wages.lm$fitted.values, frequency=12, start=c(1981, 7), end=c(1987, 6))
lines(model, col='red')
plot(y=rstandard(wages.lm), x=as.vector(time(wages)), type = 'o', ylab = 'Standardized Residuals', xlab = 'Time (Months)', main='Standardized Residual Plot')
par(mfrow = c(2, 2))
plot(wages.lm)

#Quadratic model trend
wages.qm = lm(wages ~ time(wages) + I(time(wages)^2))
summary(wages.qm)
model = ts(wages.qm$fitted.values, frequency=12, start=c(1981, 7), end=c(1987, 6))
par(mfrow=c(2, 2))
lines(model, col='red')
plot(y=rstandard(wages.qm), x=as.vector(time(wages)), type = 'o', ylab = 'Standardized Residuals', xlab = 'Time (Months)', main='Standardized Residual Plot')
par(mfrow = c(2, 2))
plot(wages.qm)

#Residual Diagnostics
par(mfrow=c(1, 3))
hist(wages.qm$residuals, main="Residuals Histogram", xlab='Residuals')
qqnorm(wages.qm$residuals, main="QQ Plot for Residuals") 
qqline(wages.qm$residuals, col="Red")
boxplot(wages.qm$residuals, main='Residual Boxplot', ylab='Residuals')

#ACF
acf(wages.qm$residuals, main='ACF of the Residuals')  

#Residual Normality
shapiro.test(wages.qm$residuals)
runs.test(wages.qm$residuals, exact=TRUE)
