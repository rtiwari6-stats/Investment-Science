
## ----echo=FALSE----------------------------------------------------------
#########################################################################
# Copyright (C) 2011-2014 Guy Yollin                                    #
# License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher #
#########################################################################

## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE,cache=FALSE,size='scriptsize',
  fig.path='figures/',fig.show='hide',fig.keep='last',
  fig.align='center',	fig.width=7,	fig.height=5,
  message=FALSE,warning=FALSE)

## ----echo=FALSE,cache=FALSE----------------------------------------------
options(width=79,continue=" ",digits=8)


## ----lret,echo=TRUE,fig.height=5,fig.width=5-----------------------------
log( 1 + c(0.1, 0.05, 0.01, 0, -0.01, -0.05, -0.1) )
x <- seq(-0.2,0.2,len=100)
x1 <- log(1+x)
plot(x=x, y=x1, xlab="x", ylab="", lwd=2, type="l", col=2)
lines(x=x, y=x, col=4, lty=4, lwd=2)
title(main="Comparison of log(1+x) and x")
legend(x=0.05, y=-0.1, c("log(1+x)","x"), col=c(2, 4), lty = c(1, 4), lwd=2)


## ----CONE,echo=TRUE,fig.height=5,fig.width=6-----------------------------
S0 <- 0
mu <- 0.5
sigma <- 1
tm <- seq(0, 10, len=100)
S <- S0 + mu*tm
ubound <- S0 + mu*tm + sigma*sqrt(tm)
lbound <- S0 + mu*tm - sigma*sqrt(tm)
ylim = range(c(ubound, lbound))
plot(x=tm, y=S, ylim=ylim, xlab="time", ylab="log price", main="random walk",
  type="l", lwd=2)
lines(tm, ubound, lty=2, lwd=2)
lines(tm, lbound, lty=3, lwd=2)
legend(3, max(ylim), c("mean","mean + sd","mean - sd"), lty = c(1,2,3), lwd=2)


## ----CONE2,echo=TRUE,fig.height=5,fig.width=6----------------------------
set.seed(2)
nsim <- 200

mat <- matrix( rnorm(n=10*nsim, mean=mu, sd=sigma), nrow=nsim, ncol=10 )
mat <- cbind(0, mat)
mat1 <- apply(mat, 1, cumsum)

matplot(x=0:10, y=mat1, type="l",
  col=rgb(0,0,100,50,maxColorValue=255),
  lty=1, xlab="time", ylab="log price")

ub <- S0 + mu*tm + 2*sigma*sqrt(tm)
lb <- S0 + mu*tm - 2*sigma*sqrt(tm)
lines(x=tm, y=ub, col=2, lwd=3)
lines(x=tm, y=lb, col=2, lwd=3)
lines(x=tm, y=S, type="l", col=1, lty=2, lwd=3)

l.str1 <- expression("E["*p[T]*"]")
l.str2 <- expression("95% Conf. Int. of "*p[T])
legend(x="topleft", legend=c(l.str1,l.str2), col=c(1,2), lty=c(2,1),
  bty="n", lwd=c(3,2), y.intersp=1.5, cex=1.0)

title("Random walk simulations")


## ----size='tiny'---------------------------------------------------------
P <- c(265.50, 264.27, 266.49, 253.81, 269.20, 277.69, 301.22, 280.98, 312.64,
  364.03, 393.62, 398.79)

P[-length(P)]

P[-1]

(R <- P[-1] / P[-length(P)] - 1)


## ------------------------------------------------------------------------
args(diff.default)
diff(P)

(R <- diff(P) / P[-length(P)])


## ----size='tiny'---------------------------------------------------------
log(1+R)

(r <- diff(log(P)))

exp(r) - 1


## ------------------------------------------------------------------------
library(zoo)
(z <- zooreg(P, as.yearmon("2013-01"), freq = 12))
class(z)
(R.z <- z[-1] / z[-length(z)] - 1)


## ------------------------------------------------------------------------
args(getS3method("lag","zoo"))
R
(R.z <- diff(z) / lag(z,-1))


## ------------------------------------------------------------------------
r
(r.z <- diff(log(z)))


## ------------------------------------------------------------------------
library(xts)
(x <- as.xts(z))
class(x)


## ------------------------------------------------------------------------
args(lag.xts)
(R.x <- diff(x) / lag(x))


## ------------------------------------------------------------------------
r
(r.x <- diff(log(x)))


