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
plot(fle.data, main='Time Series Plot of French Life Expectancy', xlab='Time (Years)', ylab='Annual Average Life Expectancy (years)')
# Stationarity Check
adf.test(fle.data)

# Quadratic fit over original data
fle.fit = lm(fle.data ~ time(fle.data) + I(time(fle.data)^2))
fle.residuals = rstudent(fle.fit)
summary(fle.fit)
fle.model = ts(fle.fit$fitted.values, frequency=1, start=1816, end=2019)

plot(fle.data, main='Quadratic Fit: Life Expectancy Data')
lines(fle.model, col='red')
legend(1850, 70, legend=c("Actual", "Fitted"), col=c("Black", "Red"), lty=1:1, cex=0.7)

# Transformed Data
fle.bc.lambda = BoxCox.lambda(fle.data)
fle.bc.tr = BoxCox(fle.data, lambda=fle.bc.lambda)
plot(fle.bc.tr, main='Box Cox Transformed Data')

fle.bc.fit = lm(fle.bc.tr ~ time(fle.bc.tr) + I(time(fle.bc.tr)^2))
fle.bc.model = ts(fle.bc.fit$fitted.values, frequency=1, start=1816, end=2019)
summary(fle.bc.fit)

plot(fle.bc.tr, main='Box Cox Transformed Data')
lines(fle.bc.model, col='red')
legend(1850, 2500, legend=c("Actual", "Fitted"), col=c("Black", "Red"), lty=1:1, cex=0.7)

# Model assessment for the transformed data
fle.bc.residuals = rstudent(fle.bc.fit)

# Detrending of the original data
fle.detrend = diff(fle.data, differences=2)
plot(fle.detrend, main='Detrended FLE Data')
# Stationarity Check
adf.test(fle.detrend)

par(mfrow=c(3, 1))
# ACF, PACF, and EACF for Detrended FLE Data
acf(fle.detrend, main="ACF: Detrended FLE Data")
pacf(fle.detrend, main="PACF: Detrended FLE Data")
eacf(fle.detrend)
plot(armasubsets(fle.detrend, 5, 5))

# ARIMA Model Fits for Detrended FLE Data
fle.detrend.arima = arima(fle.detrend, order=c(1, 0, 2))
fle.auto.arima = auto.arima(fle.detrend)
fle.bc.auto.arima = auto.arima(fle.detrend, lambda="auto")

# Residual Diagnostics (manually repeated for all the models)
par(mfrow=c(1, 3))
dataset = fle.detrend
model = fle.auto.arima
# st_residuals = fle.bc.residuals
st_residuals = rstandard(model)
plot(y=st_residuals, x=as.vector(time(dataset)), type = 'o', ylab = 'Standardized Residuals', xlab = 'Time', main='Standardized Residual Plot')
hist(st_residuals, main="Residuals Histogram", xlab='Residuals')
qqnorm(st_residuals, main="QQ Plot for Residuals") 
qqline(st_residuals, col="Red")
boxplot(st_residuals, main='Residual Boxplot', ylab='Residuals')
acf(st_residuals, main='ACF Plot')
tsdiag(model)
# Residual Normality
shapiro.test(st_residuals)
snpar::runs.test(st_residuals, exact=TRUE)
# AIC and BIC tests
AIC(model)
BIC(model)
confint(fle.bc.auto.arima)