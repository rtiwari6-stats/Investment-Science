#Code to compute daily returns, sample means, variance, skewness and kurtosis of MSFT, AMZN and GOOG.

rm(list=ls())

install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)

setwd("~/UW/CFRM-501/HW2")

#MSFT

msft_d = read.csv("msft_d.csv") # set present wd correctly

n = nrow(msft_d)

msft_d_returns = (msft_d$Adj.Close[2:n]-msft_d$Adj.Close[1:n-1])/msft_d$Adj.Close[1:n-1]

mean(msft_d_returns)

var(msft_d_returns)

kurtosis(msft_d_returns)+3

skewness(msft_d_returns)

hist(msft_d_returns, ylim=c(0,40), xlim = c(-0.08,0.08), breaks=55, freq = F, ylab="f(x)", xlab="x", 
main="Estimated Distribution of Returns - MSFT - Nov. 1 2015 - Sept. 30 2020")

x = seq(min(msft_d_returns), max(msft_d_returns), length=n)
normal = dnorm(x, mean=mean(msft_d_returns), sd=sd(msft_d_returns))
lines(x, normal)

#AMZN

amzn_d = read.csv("amzn_d.csv") # set present wd correctly

n = nrow(amzn_d)

amzn_d_returns = (amzn_d$Adj.Close[2:n]-amzn_d$Adj.Close[1:n-1])/amzn_d$Adj.Close[1:n-1]

mean(amzn_d_returns)

var(amzn_d_returns)

kurtosis(amzn_d_returns)+3

skewness(amzn_d_returns)

hist(amzn_d_returns, ylim=c(0,40), xlim = c(-0.08,0.08), breaks=55, freq = F, ylab="f(x)", xlab="x", 
     main="Estimated Distribution of Returns - AMZN - Nov. 1 2015 - Sept. 30 2020")

x = seq(min(amzn_d_returns), max(amzn_d_returns), length=n)
normal = dnorm(x, mean=mean(amzn_d_returns), sd=sd(amzn_d_returns))
lines(x, normal)

#GOOG

goog_d = read.csv("goog_d.csv") # set present wd correctly

n = nrow(goog_d)

goog_d_returns = (goog_d$Adj.Close[2:n]-goog_d$Adj.Close[1:n-1])/goog_d$Adj.Close[1:n-1]

mean(goog_d_returns)

var(goog_d_returns)

kurtosis(goog_d_returns)+3

skewness(goog_d_returns)

hist(goog_d_returns, ylim=c(0,40), xlim = c(-0.08,0.08), breaks=55, freq = F, ylab="f(x)", xlab="x", 
     main="Estimated Distribution of Returns - GOOG - Nov. 1 2015 - Sept. 30 2020")

x = seq(min(goog_d_returns), max(goog_d_returns), length=n)
normal = dnorm(x, mean=mean(goog_d_returns), sd=sd(goog_d_returns))
lines(x, normal)

