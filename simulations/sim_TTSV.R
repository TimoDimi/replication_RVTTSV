
sim_TTSV <- function(days, lambda_det, varsigma_det, TT=23401){
  
  ### 1. Simulate the stochastic multiplicator for varsigma* and lambda* 
  
  # Simulation parameters
  alpha_l <- -0.0002
  beta_1_l <- 0.01
  
  alpha_s <- -0.0002
  beta_1_s <- 0.005
  
  # Loop over the simulation days
  intensities <- tibble()
  prices <- tibble()
  for (i_day in 1:days){
    dt <- 1
    dW1 <- rnorm(TT)
    dW2 <- rnorm(TT)
    
    # Simulate the stochastic factors of lambda and varsigma
    varsigma_star <- rep(NA, TT)
    lambda_star <- rep(NA, TT)
    varsigma_star[1] <- rnorm(1, mean=0, sd=sqrt(-1/(2*alpha_s)))
    lambda_star[1] <- rnorm(1, mean=0, sd=sqrt(-1/(2*alpha_l)))
    
    for (tt in 1:(TT-1)){
      varsigma_star[tt+1] <- varsigma_star[tt] + alpha_s * varsigma_star[tt] * dt + dW1[tt]
      lambda_star[tt+1] <- lambda_star[tt] + alpha_l * lambda_star[tt] * dt + dW2[tt]
    }
    
    varsigma <- exp(-5 + beta_1_s * varsigma_star)
    varsigma <- varsigma/mean(varsigma)
    
    lambda <- exp(-5 + beta_1_l * lambda_star)
    lambda <- lambda/mean(lambda)
    
    
    # summarize as a tibble
    intensities_day <- tibble(Date=i_day,
                              SecSinceStart = 0:(TT-1),
                              lambda = lambda * lambda_det,
                              varsigma = varsigma * varsigma_det) %>%
      mutate(Lambda=cumsum(lambda))

  
  
    ### 2. Simulate an inhomogenuous Poisson process by transformation, 
    # see e.g., Alogrithm 3 by https://www.r-bloggers.com/2012/12/generating-a-non-homogeneous-poisson-process/
    Lambda_T <- tail(intensities_day$Lambda,1)
    n <- rpois(1,Lambda_T)
    
    Ft <- cumsum(intensities_day$lambda)/Lambda_T
    Ft_inv <- approxfun(x=Ft,
                        y=intensities_day$SecSinceStart,
                        rule=2)
  
  
    ### 2. Simulate the prices in the TTSV model
    # Interpolate the (simulated) varsigma at the (simulated) trading times 
    prices_day <- tibble(Date=i_day,
                         SecSinceStart=sort(Ft_inv(runif(n)))) %>%
      mutate(varsigma = approx(x=intensities_day$SecSinceStart, 
                               y=intensities_day$varsigma, 
                               xout=SecSinceStart)$y,
             lambda =  approx(x=intensities_day$SecSinceStart, 
                              y=intensities_day$lambda, 
                              xout=SecSinceStart)$y,
             LogPrice = cumsum(varsigma * rnorm(n)),
             Price = exp(LogPrice))
    
    
    intensities <- rbind(intensities, intensities_day)
    prices <- rbind(prices, prices_day)
  }
                       
  return(list(intensities=intensities, prices=prices))
}






