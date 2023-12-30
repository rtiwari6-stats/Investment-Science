#code for homework 4

rm(list=ls())

library("IntroCompFinR")
library("zoo")
library("PerformanceAnalytics")
library("PortfolioAnalytics")


setwd("~/UW/CFRM-501/HW4")

daily_returns = function(prices){
  n = nrow(prices)
  d_returns = (prices$Adj.Close[2:n]-prices$Adj.Close[1:n-1])/prices$Adj.Close[1:n-1];
  return(d_returns)
}

annualized_mean = function(prices){
  daily_mean = mean(daily_returns(prices))
  annual_mean = (daily_mean + 1) ^ 252 - 1
  return(annual_mean)
}

annualized_var = function(prices){
  daily_var = var(daily_returns(prices))
  annual_var = (daily_var + (mean(daily_returns(prices))+1)^2)^252-
                  (mean(daily_returns(prices)+1))^504
}

annualized_covariance_matrix = function(aapl, bbby, ebay, ge, goog, intc, mmm, msft, orcl,
                             teva, scale_cor){
  returns_matrix = cbind(daily_returns(aapl), daily_returns(bbby), daily_returns(ebay),
                         daily_returns(ge), daily_returns(goog), daily_returns(intc),
                         daily_returns(mmm), daily_returns(msft), daily_returns(orcl),
                         daily_returns(teva))
  
  cor_matrix = cor(returns_matrix)
  for(i in 1:dim(cor_matrix)[1]){
      for(j in 1:dim(cor_matrix)[2]){
          if( i != j){
              cor_matrix[i,j] = cor_matrix[i,j] + scale_cor
          }
      }
  }
  cov_matrix = matrix(, nrow = 10, ncol = 10)
  
  
  daily_means = c(mean(daily_returns(aapl)), mean(daily_returns(bbby)), mean(daily_returns(ebay)),
                  mean(daily_returns(ge)), mean(daily_returns(goog)),
                  mean(daily_returns(intc)), mean(daily_returns(mmm)),
                  mean(daily_returns(msft)), mean(daily_returns(orcl)),
                  mean(daily_returns(teva)))
  
  daily_sd = c(sd(daily_returns(aapl)), sd(daily_returns(bbby)), sd(daily_returns(ebay)),
               sd(daily_returns(ge)), sd(daily_returns(goog)),
               sd(daily_returns(intc)), sd(daily_returns(mmm)),
               sd(daily_returns(msft)), sd(daily_returns(orcl)),
               sd(daily_returns(teva)))
  
  for(i in 1:dim(cov_matrix)[1]) {
    for(j in 1:dim(cov_matrix)[2]) {
      cov_matrix[i,j] = (cor_matrix[i,j]*daily_sd[i]*daily_sd[j] + (daily_means[i]+1)*(daily_means[j]+1))^252 - 
        (daily_means[i]+1)^252*(daily_means[j]+1)^252
    }
  }
  return(cov_matrix)
}

aapl = read.csv("aapl.csv")
bbby = read.csv("bbby.csv")
ebay = read.csv("ebay.csv")
ge = read.csv("ge.csv")
goog = read.csv("goog.csv")
intc = read.csv("intc.csv")
mmm = read.csv("mmm.csv")
msft = read.csv("msft.csv")
orcl = read.csv("orcl.csv")
teva = read.csv("teva.csv")

asset.names = c("aapl", "bbby", "ebay", "ge", "goog","intc","mmm", "msft","orcl","teva")
annualized_sample_means = c(annualized_mean(aapl), annualized_mean(bbby), annualized_mean(ebay), 
                 annualized_mean(ge),annualized_mean(goog), annualized_mean(intc), 
                 annualized_mean(mmm),annualized_mean(msft), annualized_mean(orcl),
                 annualized_mean(teva))
names(annualized_sample_means) = asset.names

annualized_sample_var = c(annualized_var(aapl), annualized_var(bbby), annualized_var(ebay), 
                          annualized_var(ge),annualized_var(goog), annualized_var(intc), 
                          annualized_var(mmm),annualized_var(msft), annualized_var(orcl),
                          annualized_var(teva))
names(annualized_sample_var) = asset.names

annualized_sample_sd = sqrt(annualized_sample_var)
names(annualized_sample_sd) = asset.names

annualized_cov_matrix = annualized_covariance_matrix(aapl, bbby, ebay, ge, goog,
                                                     intc, mmm, msft, 
                                          orcl, teva, scale_cor = 0.0)
dimnames(annualized_cov_matrix) = list(asset.names, asset.names)

#global minimum variance portfolio
global_min_var_portfolio = globalMin.portfolio(annualized_sample_means, 
                                               annualized_cov_matrix)


ef_base = efficient.frontier(er = annualized_sample_means, 
                        cov.mat = annualized_cov_matrix, 
                        alpha.min=-2.5,
                        alpha.max=2.75, 
                        nport = 2500)

plot(ef_base, plot.assets = T, col="red")

#plot global minimum variance portfolio
points(global_min_var_portfolio$sd, global_min_var_portfolio$er, col="yellow")

#change all pairwise correlations by -0.1
annualized_cov_matrix_scaled_minus_0.1 = annualized_covariance_matrix(aapl, bbby, ebay, ge, goog,
                                                                intc, mmm, msft, 
                                                                orcl, teva, scale_cor = -0.1)

global_min_var_portfolio_scaled_minus_0.1 = globalMin.portfolio(annualized_sample_means, 
                                                          annualized_cov_matrix_scaled_minus_0.1)


ef_scaled_minus_0.1 = efficient.frontier(er = annualized_sample_means, 
                        cov.mat = annualized_cov_matrix_scaled_minus_0.1, 
                        alpha.min=-2.5,
                        alpha.max=2.75, 
                        nport = 2500)

lines(ef_scaled_minus_0.1$sd, ef_scaled_minus_0.1$er, col = "darkgreen", type="b")


points(global_min_var_portfolio_scaled_minus_0.1$sd, 
       global_min_var_portfolio_scaled_minus_0.1$er, col="orange")


#add a legend
legend(0,1.4, c("with  rho", "with rho - 0.1"), col=c("red","darkgreen"), lty=1)


#print global min porfolio
global_min_var_portfolio$er
global_min_var_portfolio$sd
global_min_var_portfolio$weights
