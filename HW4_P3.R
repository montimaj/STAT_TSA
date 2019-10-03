### STAT 5814 HW4/PROBLEM 3
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087


library(TSA)
library(forecast)
library(snpar)
library(Rfit)

wd = "./Data/HW3_Data/"
setwd(wd)
ibm_stock = read.table('internet.txt')
internet = read.table('ibm.txt')
gasprices = read.table('gasprices.txt')

### IBM Stock Data Modeling
ibm_stock.fit = auto.arima(ibm_stock)
summary(ibm_stock.fit)
ibm_stock.transformed.fit <- auto.arima(ibm_stock,lambda="auto")
summary(ibm_stock.transformed.fit)

par(mfrow=c(2,2))
plot(ibm_stock.fit$residuals, ylab="Standardized Residuals", type='l', main='Standardized Residual Plot')
abline(h = 0)
hist(ibm_stock.fit$residuals, main="Model Residual Histogram", xlab="Residual")
qqnorm(ibm_stock.fit$residuals, main="QQ Plot for Residuals") 
qqline(ibm_stock.fit$residuals, col="red")
acf(ibm_stock.fit$residuals, main="IBM Stock Residual ACF")

shapiro.test(ibm_stock.fit$residuals)
runs.test(ibm_stock.fit$residuals, exact=TRUE)

par(mfrow=c(2,2))
plot(ibm_stock.transformed.fit$residuals, ylab="Standardized Residuals", type='l', main='Standardized Residual Plot After Transformation')
abline(h = 0)
hist(ibm_stock.transformed.fit$residuals, main="Transformed Model Residual Histogram", xlab="Residual")
qqnorm(ibm_stock.transformed.fit$residuals, main="QQ Plot for Residuals After Transformation") 
qqline(ibm_stock.transformed.fit$residuals, col="red")
acf(ibm_stock.transformed.fit$residuals, main="Transformed IBM Stock Residual ACF")

shapiro.test(ibm_stock.transformed.fit$residuals)
runs.test(ibm_stock.transformed.fit$residuals, exact=TRUE)

### Internet Data Modeling
internet.fit = auto.arima(internet)
summary(internet.fit)
internet.transformed.fit <- auto.arima(internet,lambda="auto")
summary(internet.transformed.fit)

### Gasprices Data Modeling
gasprices.fit = auto.arima(gasprices)
summary(gasprices.fit)
gasprices.transformed.fit <- auto.arima(gasprices,lambda="auto")
summary(gasprices.transformed.fit)

par(mfrow=c(2,2))
plot(gasprices.fit$residuals, ylab="Standardized Residuals", type='l', main='Standardized Residual Plot')
abline(h = 0)
hist(gasprices.fit$residuals, main="Model Residual Histogram", xlab="Residual")
qqnorm(gasprices.fit$residuals, main="QQ Plot for Residuals") 
qqline(gasprices.fit$residuals, col="red")
acf(gasprices.fit$residuals, main="Gas Prices Residual ACF")

shapiro.test(gasprices.fit$residuals)
runs.test(gasprices.fit$residuals, exact=TRUE)

par(mfrow=c(2,2))
plot(gasprices.transformed.fit$residuals, ylab="Standardized Residuals", type='l', main='Standardized Residual Plot After Transformation')
abline(h = 0)
hist(gasprices.transformed.fit$residuals, main="Transformed Model Residual Histogram", xlab="Residual")
qqnorm(gasprices.transformed.fit$residuals, main="QQ Plot for Residuals After Transformation") 
qqline(gasprices.transformed.fit$residuals, col="red")
acf(gasprices.transformed.fit$residuals, main="Transformed Gas Prices Residual ACF")

shapiro.test(gasprices.transformed.fit$residuals)
runs.test(gasprices.transformed.fit$residuals, exact=TRUE)

