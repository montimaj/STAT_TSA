### STAT 5814 MIDTERM 2/PROBLEM 2
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(snpar)
library(stats)
library(forecast)

data("deere3")
par(mfrow = c(1, 1))
plot(deere3, type='o', ylab='Measurements', xlab='Time', main='Deere3 Data Plot')

# Conditional least square estimate for AR(1) model
css_model_ar1 = arima(deere3, order=c(1, 0, 0), method='CSS')
css_model_ar1_coef = coef(css_model_ar1)
print(css_model_ar1_coef)
confint(css_model_ar1)


# Maximum Likelihood estimate for AR(1) model
ml_model_ar1 = arima(deere3, order=c(1, 0, 0), method='ML')
ml_model_ar1_coef = coef(ml_model_ar1)
print(ml_model_ar1_coef)
confint(ml_model_ar1)

# Conditional least square estimate for AR(2) model
css_model_ar2 = arima(deere3, order=c(2, 0, 0), method='CSS')
css_model_ar2_coef = coef(css_model_ar2)
print(css_model_ar2_coef)
confint(css_model_ar2)

# Maximum Likelihood estimate for AR(2) model
ml_model_ar2 = arima(deere3, order=c(2, 0, 0), method='ML')
ml_model_ar2_coef = coef(ml_model_ar2)
print(ml_model_ar2_coef)
confint(ml_model_ar2)

# Residual Diagnostics (manually repeated for all the models)
par(mfrow=c(3, 2))
model = ml_model_ar2
st_residuals = rstandard(model)
plot(y=st_residuals, x=as.vector(time(deere3)), type = 'o', ylab = 'Standardized Residuals', xlab = 'Time', main='Standardized Residual Plot')
hist(st_residuals, main="Residuals Histogram", xlab='Residuals')
qqnorm(st_residuals, main="QQ Plot for Residuals") 
qqline(st_residuals, col="Red")
boxplot(st_residuals, main='Residual Boxplot', ylab='Residuals')
acf(st_residuals, main='ACF Plot')
#Residual Normality
shapiro.test(st_residuals)
runs.test(st_residuals, exact=TRUE)

#AIC and BIC tests
AIC(model)
BIC(model)

#Confirmation
auto_arima = auto.arima(deere3)
auto_arima