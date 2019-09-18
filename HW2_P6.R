### STAT 5814 HW2/PROBLEM 6
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)
library(latex2exp)

par(mfrow=c(1, 1))
data(prescrip)
plot(prescrip, ylab='Monthly Sales', type='l', main='Time Series Plot for Prescription Data')
points(y=prescrip,x=time(prescrip), pch=as.vector(season(prescrip)))

zlag_prescrip = zlag(prescrip)
percentage=na.omit((prescrip - zlag_prescrip) / zlag_prescrip)
diff_prescrip = diff(prescrip)
corr = round(cor(diff_prescrip[-1], percentage[-1]), 3)
plot(x=diff_prescrip[-1], y=percentage[-1], xlab=TeX('$\\nabla{X_t} = X_t - X_{t-1}\\, (First\\,Differences)$'),
     ylab=TeX('$(X_t - X_{t-1}) / X_{t-1}\\,(Fractional\\,Relaive\\,Change)$'), main=paste('Plot for First Differences, Correlation = ', corr))

qqnorm(diff_prescrip[-1], main='QQ Plot for First Differences')
qqline(diff_prescrip[-1])
shapiro.test(diff_prescrip[-1])
