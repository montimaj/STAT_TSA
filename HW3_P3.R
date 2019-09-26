### STAT 5814 HW3/PROBLEM 3
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(forecast)
library(snpar)
library(Rfit)

tb = read.table("/home/montimaj/Documents/MST/STAT_5814/HW/STAT_TSA/Data/HW3_Data/tb.txt")
tb_data = ts(tb, frequency=12, start=c(2000, 1), end=c(2009, 12))
decomposed_tb = decompose(tb_data, type="mult")  
plot(decomposed_tb)

har. <- harmonic(tb_data, 1)
fit <- lm(tb_data ~ har.+time(tb_data))
tb_data.residuals <- rstudent(fit)
summary(fit) 

par(mfrow=c(1,1))
model = ts(fit$fitted.values, frequency=12, start=c(2000,1), end=c(2009,12))
plot(tb_data, main='Plot for TB data', ylab='TB', type="l", col="Blue")
lines(model, col="Red")
legend(2008, 1400, legend=c("Actual", "Fitted"), col=c("Blue", "Red"), lty=1:1, cex=0.55)

tb_data.residuals = rstudent(fit)
par(mfrow=c(2,2))
hist(tb_data.residuals, main="Residuals Histogram")
qqnorm(tb_data.residuals, main="QQ Plot for Residuals") 
qqline(tb_data.residuals, col="Red")
acf(tb_data.residuals, main='TB Residuals')   
plot(tb_data.residuals, ylab="Standardized Residuals", type='l', main='Plot for Standardized Residuals')
abline(h = 0)

shapiro.test(tb_data.residuals)
runs.test(tb_data.residuals, exact=TRUE)