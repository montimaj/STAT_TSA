### STAT 5814 HW3/PROBLEM 1
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(forecast)
library(snpar)

wd = "/home/montimaj/Documents/MST/STAT_5814/HW/STAT_TSA/Data/HW3_Data"
setwd(wd)
gasprices = read.table('gasprices.txt')
internet = read.table('ibm.txt')
ibm_stock = read.table('internet.txt')

par(mfrow=c(2,2))
plot(c(1:nrow(gasprices)),
     gasprices$V1, 
     xlab="Time (Weeks)",
     ylab="Gas Price (USD/Gallon)",
     type="l",
     main="Gas Prices")
plot(c(1:nrow(ibm_stock)),
     ibm$V1, 
     type="l",
     ylab="Stock Prices (USD)",
     xlab="Time (Days)",
     main="IBM Stock")
plot(c(1:nrow(internet)), 
     internet$V1, 
     type="l",
     ylab="Number of Loggedin Users",
     xlab="Time (Minute)",
     main="Internet Users")

par(mfrow=c(2,2))
max_lag = 40
gasprices.acf = acf(gasprices, plot=TRUE, lag.max=max_lag, main="ACF Gas Prices")
ibm_stock.acf = acf(ibm_stock, plot=TRUE, lag.max=max_lag, main="IBM Stock ACF")
internet.acf = acf(internet, plot=TRUE, lag.max=max_lag, main = "Internet ACF")

### GASPRICES DATA
gasprices.train = ts(gasprices, start=1, end=125) 
gasprices.test = ts(gasprices, start=126, end=145)
gasprices.fit = auto.arima(gasprices.train)
summary(gasprices.fit)

gasprices.residuals = rstandard(gasprices.fit)
par(mfrow=c(2,2))
plot(gasprices.residuals, ylab="Standardized Residuals", type='l', main='Standardized Residual Plot')
abline(h = 0)
hist(gasprices.residuals, main="Model Residual Histogram", xlab="Residual")
qqnorm(gasprices.residuals, main="QQ Plot for Residuals") 
qqline(gasprices.residuals, col="red")
acf(gasprices.residuals, main="Gas Prices Residual ACF")

shapiro.test(gasprices.residuals)
runs.test(gasprices.residuals, exact=TRUE)

### IBM STOCK DATA
ibm_stock.train = ts(ibm_stock, start=1, end=80) 
ibm_stock.test = ts(ibm_stock, start=81, end=100)
ibm_stock.fit = auto.arima(ibm_stock.train)
summary(ibm_stock.fit)

ibm_stock.residuals = rstandard(ibm_stock.fit)
par(mfrow=c(2,2))
plot(ibm_stock.residuals, ylab="Standardized Residuals", type='l', main='Standardized Residual Plot')
abline(h = 0)
hist(ibm_stock.residuals, main="Model Residual Histogram", xlab="Residual")
qqnorm(ibm_stock.residuals, main="QQ Plot for Residuals") 
qqline(ibm_stock.residuals, col="red")
acf(ibm_stock.residuals, main="IBM Stock Residual ACF")

shapiro.test(ibm_stock.residuals)
runs.test(ibm_stock.residuals, exact=TRUE)

### INTERNET DATA
internet.train = ts(internet, start=1, end=80) 
internet.test = ts(internet, start=81, end=100)
internet.fit = auto.arima(internet.train)
summary(internet.fit)

internet.residuals = rstandard(internet.fit)
par(mfrow=c(2,2))
plot(internet.residuals, ylab="Standardized Residuals", type = 'l', main='Standardized Residual Plot')
abline(h = 0)
hist(internet.residuals, main="Model Residual Histogram", xlab="Residual")
qqnorm(internet.residuals, main="QQ Plot for Residuals") 
qqline(internet.residuals, col="red")
acf(internet.residuals, main="Internet Residual ACF")

shapiro.test(internet.residuals)
runs.test(internet.residuals, exact=TRUE)
