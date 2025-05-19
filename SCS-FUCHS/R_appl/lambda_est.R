#   The function lambda_est estimates the intensity process lambda based on tick-times.
#   It computes a possibly bias-
#   corrected kernel estimate of the intensity lambda in the interval [0,T] 
#   based on the timing of transactions given in t_tick. 

lambda_est <- function(tau_grid, t_tick, TT, k=k_Epanechnikov, h=1000){
  
  NT <- length(t_tick)
  lambda <- rep(NA, length(tau_grid))
  
  for (index_tau in 1:length(tau_grid)){
    tau <- tau_grid[index_tau]
    # Estimate lambda nonparametrically
    lambda[index_tau] <- 1/h * sum(k((t_tick - tau)/h))
    
    # These two terms are possible bias corrections
    if (tau < h) {lambda[index_tau] <- lambda[index_tau] + 1/h * sum(k((t_tick + tau)/h)) }
    if (tau >  TT - h) {lambda[index_tau] <- lambda[index_tau] + 1/h * sum(k((t_tick + tau - 2*TT)/h)) }
  }
  
  lambda
}


# Epanechnikov kernel function
k_Epanechnikov <- function(x){
  3/4 * (1-x^2) * (abs(x) <= 1)
}

