### STAT 5814 HW2/PROBLEM 1
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(latex2exp)

par(mfrow=c(2, 2))

lag = 10
ACF = ARMAacf(ar=.6, lag.max=lag)
plot(y=ACF[-1], x=1:lag, xlab='Lag', ylab='ACF', type='h', main=TeX('$\\mathbf{AR(1)\\,for\\, \\phi = 0.6}$'))
abline(h=0)

ACF = ARMAacf(ar=-.6, lag.max=lag)
plot(y=ACF[-1], x=1:lag, xlab='Lag', ylab='ACF', type='h', main=TeX('$\\mathbf{AR(1)\\,for\\, \\phi = -0.6}$'))
abline(h=0)

ACF = ARMAacf(ar=.3, lag.max=lag)
plot(y=ACF[-1], x=1:lag, xlab='Lag', ylab='ACF', type='h', main=TeX('$\\mathbf{AR(1)\\,for\\, \\phi = 0.3}$'))
abline(h=0)

lag=20
ACF = ARMAacf(ar=.95, lag.max=lag)
plot(y=ACF[-1], x=1:lag, xlab='Lag', ylab='ACF', type='h', main=TeX('$\\mathbf{AR(1)\\,for\\, \\phi = 0.95}$'))
abline(h=0)
