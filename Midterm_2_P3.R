### STAT 5814 MIDTERM 2/PROBLEM 3
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(snpar)
library(stats)
library(forecast)
library(tseries)
library(fUnitRoots)

data = read.table('Data/ExamData/petr.txt', header=T)
chemicals.data = ts(data$Chemicals, frequency=12, start=c(1971, 1), end=c(1991, 12))
vehicles.data = ts(data$Vehicles, frequency=12, start=c(1971, 1), end=c(1991, 12))
par(mfrow=c(1, 1))
plot(chemicals.data, main='Time Series Plot of Chemicals Data')
plot(vehicles.data, main='Time Series Plot of Vehicles Data')
chemicals.decompose = decompose(chemicals.data, type="mult")  
plot(chemicals.decompose)
vehicles.decompose = decompose(vehicles.data, type="mult")  
plot(vehicles.decompose)

# Stationarity Check
adf.test(chemicals.data)
adf.test(vehicles.data)

par(mfrow=c(2, 2))
# Differencing to remove linear trend
chemicals.diff= diff(chemicals.data)
plot(chemicals.diff, main='Differencing=1: Chemicals Data')

# Differencing at lag 12 (monthly data) to remove seasonality
chemicals.diff2 = diff(chemicals.diff, lag=12)
plot(chemicals.diff2, main='Lag=12: Chemicals Data')
# Stationarity Check
adf.test(chemicals.diff2)

# Differencing to remove linear trend
vehicles.diff= diff(vehicles.data)
plot(vehicles.diff, main='Differencing=1: Vehicles Data')

# Differencing at lag 12 (monthly data) to remove seasonality
vehicles.diff2 = diff(vehicles.diff, lag=12)
plot(vehicles.diff2, main='Lag=12: Vehicles Data')
# Stationarity Check
adf.test(chemicals.diff2)

# Linear Trend and Harmonic (Seasonal) Trend for Chemicals Data
har = harmonic(chemicals.data, 1)
chemicals.fit = lm(chemicals.data ~ har + time(chemicals.data))
chemicals.residuals = rstudent(chemicals.fit)
summary(chemicals.fit)
chemicals.model = ts(chemicals.fit$fitted.values, frequency=12, start=c(1971, 1), end=c(1991, 12))

plot(chemicals.data, main='TS Plot: Chemicals Data')
lines(chemicals.model, col='red')
legend(1975, 25, legend=c("Actual", "Fitted"), col=c("Black", "Red"), lty=1:1, cex=0.7)

# Checking Transformation for Chemicals Data
chemicals.bc.lambda = BoxCox.lambda(chemicals.data)
chemicals.bc.tr = BoxCox(chemicals.data, lambda=chemicals.bc.lambda)
har = harmonic(chemicals.bc.tr, 1)
chemicals.bc.fit = lm(chemicals.bc.tr ~ (har + time(chemicals.bc.tr)))
chemicals.bc.residuals = rstudent(chemicals.bc.fit)
summary(chemicals.bc.fit)
chemicals.bc.model = ts(chemicals.bc.fit$fitted.values, frequency=12, start=c(1971, 1), end=c(1991, 12))

plot(chemicals.bc.tr, main='TS Plot: Transformed Chemicals Data')
lines(chemicals.bc.model, col='red')
legend(1975, 4, legend=c("Actual", "Fitted"), col=c("Black", "Red"), lty=1:1, cex=0.7)

# Linear Trend and Harmonic (Seasonal) Trend for Vehicles Data
har = harmonic(vehicles.data, 1)
vehicles.fit = lm(vehicles.data ~ har + time(vehicles.data))
vehicles.residuals = rstudent(vehicles.fit)
summary(vehicles.fit)
vehicles.model = ts(vehicles.fit$fitted.values, frequency=12, start=c(1971, 1), end=c(1991, 12))

plot(vehicles.data, main='TS Plot: Vehicles Data')
lines(vehicles.model, col='red')
legend(1975, 25, legend=c("Actual", "Fitted"), col=c("Black", "Red"), lty=1:1, cex=0.7)

# Checking Transformation for Vehicles Data
vehicles.bc.lambda = BoxCox.lambda(vehicles.data)
vehicles.bc.tr = BoxCox(vehicles.data, lambda=vehicles.bc.lambda)
har = harmonic(vehicles.bc.tr, 1)
vehicles.bc.fit = lm(vehicles.bc.tr ~ (har + time(vehicles.bc.tr)))
vehicles.bc.residuals = rstudent(vehicles.bc.fit)
summary(vehicles.bc.fit)
vehicles.bc.model = ts(vehicles.bc.fit$fitted.values, frequency=12, start=c(1971, 1), end=c(1991, 12))

plot(vehicles.bc.tr, main='TS Plot: Transformed Vehicles Data')
lines(vehicles.bc.model, col='red')
legend(1975, 4, legend=c("Actual", "Fitted"), col=c("Black", "Red"), lty=1:1, cex=0.7)


# Differencing after BoxCox Chemicals Data
chemicals.bc.diff= diff(chemicals.bc.tr)
plot(chemicals.bc.diff, main='Differencing=1: Transformed Chemicals Data')

# Differencing at lag 12 (monthly data) to remove seasonality
chemicals.bc.diff2 = diff(chemicals.bc.diff, lag=12)
plot(chemicals.bc.diff2, main='Lag=12: Transformed Chemicals Data')
# Stationarity Check
adf.test(chemicals.bc.diff2)

# Differencing after BoxCox Vehicles Data
vehicles.bc.diff= diff(vehicles.bc.tr)
plot(vehicles.bc.diff, main='Differencing=1: Transformed Vehicles Data')

# Differencing at lag 12 (monthly data) to remove seasonality
vehicles.bc.diff2 = diff(vehicles.bc.diff, lag=12)
plot(vehicles.bc.diff2, main='Lag=12: Transformed Vehicles Data')
# Stationarity Check
adf.test(vehicles.bc.diff2)

par(mfrow=c(2, 3))
# ACF, PACF, and EACF for Chemicals Data
acf(chemicals.bc.diff2, main="ACF: Stabilized Chemicals Data")
pacf(chemicals.bc.diff2, main="PACF: Stabilized Chemicals Data")
eacf(chemicals.bc.diff2)
plot(armasubsets(chemicals.bc.diff2, 5, 5))

# ACF, PACF, and EACF for Vehicles Data
acf(vehicles.bc.diff2, main="ACF: Stabilized Vehicles Data")
pacf(vehicles.bc.diff2, main="PACF: Stabilized Chemical Data")
eacf(vehicles.bc.diff2)
plot(armasubsets(vehicles.bc.diff2, 5, 5))

par(mfrow=c(2, 1))
# ARIMA Model Fits for Chemicals Data
chemicals.arima = auto.arima(chemicals.data)
chemicals.bc.arima = auto.arima(chemicals.data, lambda='auto')
chemicals.bc.diff2.arima = auto.arima(chemicals.bc.diff2)

# ARIMA Model Fits for Vehicles Data
vehicles.arima = auto.arima(vehicles.data)
vehicles.bc.arima = auto.arima(vehicles.data, lambda='auto')
vehicles.bc.diff2.arima = auto.arima(vehicles.bc.diff2)

# Residual Diagnostics (manually repeated for all the models)
par(mfrow=c(3, 2))
dataset = chemicals.data
model = chemicals.model
st_residuals = chemicals.residuals
# st_residuals = rstandard(model)
plot(y=st_residuals, x=as.vector(time(dataset)), type = 'o', ylab = 'Standardized Residuals', xlab = 'Time', main='Standardized Residual Plot')
hist(st_residuals, main="Residuals Histogram", xlab='Residuals')
qqnorm(st_residuals, main="QQ Plot for Residuals") 
qqline(st_residuals, col="Red")
boxplot(st_residuals, main='Residual Boxplot', ylab='Residuals')
acf(st_residuals, main='ACF Plot')
# Residual Normality
shapiro.test(st_residuals)
snpar::runs.test(st_residuals, exact=TRUE)

# AIC and BIC tests
AIC(model)
BIC(model)

# Correlation between Chemicals and Vehicles Data
ccf_cv = ccf(chemicals.data, vehicles.data)
