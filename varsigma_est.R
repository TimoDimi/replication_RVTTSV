
#   The function varsigma2_est estimates the tick volatility process varsigma2 based on tick-times and prices based on
#   the estimator called hat sigma^2_{pavg}(t_0) on bottom of page 4 on Dahlhaus and Tunyavetchakit (2016).

varsigma2_est <- function(tau_grid, t_tick, prices_tick, TT, k=k_Epanechnikov, H=40, m_factor=1000, boundary_correct=TRUE){
  # prices_tick are the log-prices,
  
  # Bandwidth parameters
  NT <- length(t_tick)
  m <- floor(m_factor * NT/TT)
  
  # help functions
  g <- function(x){ x * (1-x) * (x>=0 & x<=1) }
  g2 = 1/30 # from WolframAlpha with command:   int_0^1 (x * (1-x))^2
  
  dg <- function(x){ (1-2*x) * (x>=0 & x<=1) }
  h_fun <- function(x,y){ (g((x+1)/y) - g(x/y)) }
  
   
  # Boundary correction: simply mirror Log-Prices at the beginning and the end!
  if (boundary_correct==TRUE){
    prices_tick <- c(rev(head(prices_tick,m)),
                     prices_tick,
                     rev(tail(prices_tick,m)))
  }
  diff_prices_tick <- c(diff(prices_tick),0)  
  
  
  # Pre-averaging estimator
  Delta_Y_bar <- rep(NA, length(prices_tick))
  for (i in 1:(length(prices_tick)-H+1)){
    Delta_Y_bar[i] <- sum( g((1:(H-1))/H) * (prices_tick[(i+1):(i+H-1)] - prices_tick[(i):(i+H-2)]) )
  }
  Delta_Y_bar <- Delta_Y_bar %>% zoo::na.locf() # Fill with last value!
  

  # Actual estimator
  varsigma2 <- rep(NA, length(tau_grid))
  for (index_tau in 1:length(tau_grid)){
    tau <- tau_grid[index_tau]
    # Find smallest index i_tau corresponding to a trading time that is larger than tau
    i_ttick_tau <- min(min(which(t_tick > tau)), NT) # Simply use the last tick if there is no tick change after tau
    if (boundary_correct==TRUE){ i_ttick_tau <- i_ttick_tau + m }
    
    varsigma2[index_tau] <- 1/(m*H*g2) * sum( k(((-m):(m))/m) * (Delta_Y_bar[(i_ttick_tau-m):(i_ttick_tau+m)]^2)) -
      1/(2*m*H*g2) * sum(h_fun(1:(H-1),H)^2) * sum( k(((-m):(m))/m) * diff_prices_tick[(i_ttick_tau-m):(i_ttick_tau+m)]^2 )
  }
  
  # Make sure no negative values are reported!
  varsigma2 <- pmax(varsigma2, 10^(-10))
  
  varsigma2
}
            

