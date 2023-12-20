# This function simulates from the TTSV model with independent processes as used 
# in the simulation section of the paper.
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







# This function simulates from the TTSV model with dependent leverage-type processes 
# as used in the simulation section of the paper.
sim_TTSV_leverage <- function(days, lambda_det, varsigma_det, TT=23401){
  
  # Simulation parameters
  alpha_l <- -0.0002
  beta_1_l <- 0.01
  
  alpha_s <- -0.0002
  beta_1_s <- 0.005
  
  # Loop over the simulation days
  intensities <- tibble()
  prices <- tibble()
  for (i_day in 1:days){
    ### 1. Simulate the stochastic multiplicator for lambda* 
    lambda_star <- rep(NA, TT)
    lambda_star[1] <- rnorm(1, mean=0, sd=sqrt(-1/(2*alpha_l)))
    
    dt <- 1
    dW_lambda <- rnorm(TT)
    for (tt in 1:(TT-1)){
      lambda_star[tt+1] <- lambda_star[tt] + alpha_l * lambda_star[tt] * dt + dW_lambda[tt]
    }
    
    lambda <- exp(-5 + beta_1_l * lambda_star) # the "-5" is for numerical stability, does not matter due to the normalization below
    lambda <- lambda/mean(lambda)
    
    # summarize as a tibble
    intensities_day <- tibble(Date=i_day,
                              SecSinceStart = 0:(TT-1),
                              lambda = lambda * lambda_det) %>%
      mutate(Lambda=cumsum(lambda))
    
    
    ### 2. Simulate an inhomogenuous Poisson process by transformation, 
    # see e.g., Alogrithm 3 by https://www.r-bloggers.com/2012/12/generating-a-non-homogeneous-poisson-process/
    Lambda_T <- tail(intensities_day$Lambda,1)
    n <- rpois(1,Lambda_T)
    
    Ft <- cumsum(intensities_day$lambda)/Lambda_T
    Ft_inv <- approxfun(x=Ft,
                        y=intensities_day$SecSinceStart,
                        rule=2)
    
    
    ### 3. Simulate ticks
    prices_day <- tibble(Date=i_day,
                         SecSinceStart=sort(Ft_inv(runif(n))))
    
    
    ### 4. Simulate varsigma on the ticks (and correct volatility in its evolution!)
    
    # Correlation between Brownian motions driving sigma and the price for a leverage effect!
    n_ticks <- dim(prices_day)[1]
    rho <- -0.5
    Sigma <- rbind(c(1,rho), c(rho,1))
    dW_vec <- MASS::mvrnorm(n_ticks, mu=rep(0,2), Sigma=Sigma)
    dW_varsigma <- dW_vec[,1]
    dW_price <- dW_vec[,2]
    
    # Simulate varsigma_star (at the tick times)
    varsigma_star <- rep(NA, n_ticks)
    varsigma_star[1] <- rnorm(1, mean=0, sd=sqrt(-1/(2*alpha_s)))
    time_last_trade <- diff(prices_day$SecSinceStart)
    for (tt in 1:(n_ticks-1)){
      varsigma_star[tt+1] <- varsigma_star[tt] + alpha_s * varsigma_star[tt] * dt + sqrt(time_last_trade[tt]) * dW_varsigma[tt]
    }
    varsigma_factor <- exp(-5 + beta_1_s * varsigma_star)
    varsigma_factor <- varsigma_factor/mean(varsigma_factor)

    ### 5. Collect intensities_day
    intensities_day <- intensities_day %>%
      mutate(varsigma = varsigma_det * approx(x=prices_day$SecSinceStart,
                                              y=varsigma_factor, 
                                              xout=SecSinceStart,
                                              rul=2)$y)
    
    
    ### 6. Simulate price process and collect intensities
    prices_day <- prices_day %>%
      mutate(varsigma = approx(x=intensities_day$SecSinceStart, 
                               y=intensities_day$varsigma, 
                               xout=SecSinceStart)$y,
             lambda =  approx(x=intensities_day$SecSinceStart, 
                              y=intensities_day$lambda, 
                              xout=SecSinceStart)$y,
             LogPrice = cumsum(varsigma * dW_price),
             Price = exp(LogPrice))
    
    
    # Bind daily data frame to overall data frame
    intensities <- rbind(intensities, intensities_day)
    prices <- rbind(prices, prices_day)
  }
  
  return(list(intensities=intensities, prices=prices))
}



# This function generates a noise process "eps" with a given standard deviation,
# in either the "iid", "ARMA" or "ARMA-diurnal" options. 
# Some parameters are specified inside the function for simplicity

noise_process <- function(n_ticks, sd_eps, noise_setting="iid"){
  if (noise_setting=="iid"){
    eps <- rnorm(n_ticks, 0, sd=sd_eps)
  } else if (noise_setting=="ARMA"){
    burn_in <- 100
    phi <- 0.5
    theta <- 0.5
    sd_u <- sd_eps * sqrt( (1-phi^2) / (1 + 2*phi*theta + theta^2)) 
    u <- rnorm(n_ticks+burn_in, 0, sd=sd_u)
    eps <- rep(NA, n_ticks+burn_in)
    eps[1] <- 0
    for (tt in 2:(n_ticks+burn_in)){
      eps[tt] <- phi*eps[tt-1] + theta*u[tt-1] + u[tt]
    }
    eps <- tail(eps, n_ticks)
  } else if (noise_setting=="ARMA-diurnal"){
    # Triangular kernel that generates noise with double the variance at noon compared to morning/evening.
    kernel_diurnal <- function(x)( (4/3-4/3*x)*(0<=x & x<=0.5) + (4/3*x)*(0.5<x & x<=1))
    
    burn_in <- 100
    phi <- 0.5
    theta <- 0.5
    
    # Transformed standard deviation for u (keep the factor at 1 for the burn_in phase)
    var_u <- c(rep(sd_eps^2 * (1-phi^2) / (1 + 2*phi*theta + theta^2), burn_in),
               sd_eps^2 * (1-phi^2) / (1 + 2*phi*theta + theta^2) *  kernel_diurnal((1:n_ticks)/n_ticks))
    u <- rnorm(n_ticks+burn_in, 0, sd=sqrt(var_u))
    eps <- rep(NA, n_ticks+burn_in)
    eps[1] <- 0
    for (tt in 2:(n_ticks+burn_in)){
      eps[tt] <- phi*eps[tt-1] + theta*u[tt-1] + u[tt]
    }
    eps <- tail(eps, n_ticks)
  } else {
    eps <- rep(0, length(n_ticks))
  }
  
  return(eps)
}







