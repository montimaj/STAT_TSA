### STAT 5814 HW3/PROBLEM 2
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(forecast)
library(snpar)
library(Rfit)
library(latex2exp)

data(co2)
co2_data = ts(co2, frequency=12)
har. = harmonic(co2_data, 1)
fit = lm(co2_data ~ har.+time(co2_data))
summary(fit)

par(mfrow=c(1,1))
model = ts(fit$fitted.values, frequency=12)
plot(co2_data, main=TeX('$\\textbf{Plot\\,for\\,CO_2\\,level$}'), ylab=TeX('$CO_2\\,Level$'), type="l", col="Blue")
lines(model, col="Red")
legend(1, 380, legend=c("Actual", "Fitted"), col=c("Blue", "Red"), lty=1:1, cex=0.7)

co2_data.residuals = rstudent(fit)
par(mfrow=c(2,2))
hist(co2_data.residuals, main="Residuals Histogram")
qqnorm(co2_data.residuals, main="QQ Plot for Residuals") 
qqline(co2_data.residuals, col="Red")
acf(co2_data.residuals, main=TeX("$\\textbf{CO_2\\,Residuals\\,ACF}$"))   
plot(co2_data.residuals, ylab="Standardized Residuals", type='l', main='Plot for Standardized Residuals')
abline(h = 0)

shapiro.test(co2_data.residuals)
runs.test(co2_data.residuals, exact=TRUE)
