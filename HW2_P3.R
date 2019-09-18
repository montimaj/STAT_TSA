### STAT 5814 HW2/PROBLEM 3
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(latex2exp)

par(mfrow=c(1, 2))

lag = 20
ACF=ARMAacf(ar=0.7,ma=-0.4,lag.max=lag)
plot(y=ACF[-1], x=1:lag, xlab='Lag', ylab='ACF', type='h', main=TeX('$\\mathbf{ARMA(1, 1)\\,for\\, \\phi = 0.7, \\, \\theta = 0.4}$'))
abline(h=0)

ACF=ARMAacf(ar=0.7,ma=0.4,lag.max=lag)
plot(y=ACF[-1], x=1:lag, xlab='Lag', ylab='ACF', type='h', main=TeX('$\\mathbf{ARMA(1, 1)\\,for\\, \\phi = 0.7, \\, \\theta = -0.4}$'))
abline(h=0)
