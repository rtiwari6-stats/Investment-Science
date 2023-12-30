annualized_cov_matrix_scaled_0.1 = annualized_covariance_matrix(aapl, bbby, ebay, ge, goog,
                                                                intc, mmm, msft, 
                                                                orcl, teva, scale_cor = 0.1)

global_min_var_portfolio_scaled_0.1 = globalMin.portfolio(annualized_sample_means, 
                                                          annualized_cov_matrix_scaled_0.1)

print(global_min_var_portfolio_scaled_0.1)


ef = efficient.frontier(er = annualized_sample_means, 
                        cov.mat = annualized_cov_matrix_scaled_0.1, 
                        alpha.min=-2.5,
                        alpha.max=1.5, 
                        nport = 1000)
par(new = T)

plot(ef, plot.assets = F, col="blue", lwd = 1)


points(global_min_var_portfolio_scaled_0.1$sd, 
       global_min_var_portfolio_scaled_0.1$er, col="green")
