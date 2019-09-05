### STAT 5814 HW1/PROBLEM 8
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

library(TSA)

data_path = '/home/montimaj/Documents/MST/STAT_5814/HW/STAT_TSA/Data/gasprices.txt'
data = data.frame(read.table(data_path))
colnames(data) = c('GP')
n = length(data$GP)
data$week = c(1: n)
plot(data$week ,data$GP, xlab='Week', ylab='Gas Prices (USD)', main='Time Series Plot of Gas Prices', type = "l")

gasprices = data$GP
lag_set = c(1, 2, 3, 5, 15)
for (lag in lag_set) {
  k = lag + 1
  auto_corr = round(cor(gasprices[k:n], zlag(gasprices, lag)[k:n]), 3)
  plot(x=zlag(gasprices, lag), y=gasprices, xlab=bquote('Y'[t-~.(lag)]), ylab=expression(Y[t]), main=paste('Lag', lag, 'Scatter Plot\nAutocorrelation=', auto_corr), type='p')
}
