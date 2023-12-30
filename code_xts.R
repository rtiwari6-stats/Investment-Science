rm(list=ls())

install.packages("PerformanceAnalytics")
install.packages("quantmod")

library(PerformanceAnalytics)
library(quantmod)

setwd("~/UW/CFRM-501/HW2")

d = read.csv("amzn_d.csv") # set present wd correctly
d_xts = xts(d[,-1],  order.by=as.Date(d$Date))

d_xts_returns = dailyReturn(d_xts$Adj.Close)

mean(d_xts_returns)
sd(d_xts_returns)
kurtosis(d_xts_returns)+3
skewness(d_xts_returns)

hist(d_xts_returns, ylim=c(0,40), xlim = c(-0.05,0.05), breaks=50, freq = F, ylab="f(x)", xlab="x", 
     main="Estimated Distribution of Returns - DATA - Nov. 1 2015 - Sept. 30 2020")