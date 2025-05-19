
# ------------------ TTSV-Hawkes Simulation with Leverage ------------------

simulate_TTSV_Hawkes <- function(days=2,
                                 TT=23401,
                                 lambda_det,
                                 varsigma_det,
                                 a_pos_lambda = 0.05,
                                 a_neg_lambda = 0.1, 
                                 b_lambda = 0.5,
                                 a_pos_varsigma = 0.0001,
                                 a_neg_varsigma = 0.0002, 
                                 b_varsigma = 1,
                                 rho_intensities = 0,
                                 seed = NULL) {
  
  # Sigma_intensities <- diag(2)
  Sigma_intensities <- rbind(c(1,rho_intensities), c(rho_intensities,1))
  
  T_max <- TT-1
  
  # Error handling
  if (!is.numeric(T_max) || T_max <= 0) stop("T_max must be positive numeric.")
  
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  
  # Simulation parameters
  alpha_l <- -0.0002
  beta_1_l <- 0.005
  
  alpha_s <- -0.0002
  beta_1_s <- 0.0025
  
  
  # Loop over the simulation days
  intensities <- tibble()
  prices <- tibble()
  return_tbl <- tibble()
  for (i_day in 1:days){
    dt <- 1
    dW_joint <- MASS::mvrnorm(TT, mu=rep(0,2), Sigma=Sigma_intensities)
    dW1 <- dW_joint[,1]
    dW2 <-  dW_joint[,2]
    
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
    intensities_diffusion_day <- tibble(Date=i_day,
                                        SecSinceStart = 0:(TT-1),
                                        lambda_diff = lambda * lambda_det,
                                        varsigma_diff = varsigma * varsigma_det)
    
    
    # Initialize vectors to store event times, signs, and intensity values.
    event_times <- c()
    event_signs <- c()
    intensity_record <- c()
    varsigmas <- c()
    LogReturns <- c()
    
    # Initialize simulation time
    t <- 0
    lambda_func <- function(t) {approx(x=0:23400, y=intensities_diffusion_day$lambda_diff, xout=t, rule=2)$y}
    varsigma_func <- function(t) {approx(x=0:23400, y=intensities_diffusion_day$varsigma_diff, xout=t, rule=2)$y}
    lambda_t <- lambda_func(0)
    varsigma_t <- varsigma_func(0)
    
    while (t < T_max) {
      # Set upper bound M for thinning
      M <- lambda_t
      
      # Sample the next candidate inter-arrival time
      u <- runif(1)
      w <- -log(u) / M
      t_candidate <- t + w
      if (t_candidate > T_max) break
      
      # Compute baseline intensity at candidate time
      lambda_t_candidate <- lambda_func(t_candidate)
      varsigma_t_candidate <- varsigma_func(t_candidate)
      
      # If there was a previous event, update intensity recursively
      if (length(event_times) > 0) {
        t_last <- tail(event_times, 1)  # Most recent event time
        
        lambda_candidate <- lambda_t_candidate + (lambda_t - lambda_func(t_last)) * exp(-b_lambda * (t_candidate - t_last))
        varsigma_candidate <- varsigma_t_candidate + (varsigma_t - varsigma_func(t_last)) * exp(-b_varsigma * (t_candidate - t_last))
      } else {
        lambda_candidate <- lambda_t_candidate
        varsigma_candidate <- varsigma_t_candidate
      }
      
      # Accept candidate event with probability lambda_candidate / M
      if (runif(1) <= lambda_candidate / M) {
        t <- t_candidate
        event_times <- c(event_times, t)
        intensity_record <- c(intensity_record, lambda_candidate)
        
        # Determine event sign by drawing an independent standard normal - Insert here U_i for the price
        varsigmas <- c(varsigmas, varsigma_candidate)
        LogReturn <- rnorm(1) * varsigma_candidate
        LogReturns <- c(LogReturns, LogReturn)
        sign_event <- ifelse(LogReturn >= 0, 1, -1)
        event_signs <- c(event_signs, sign_event)
        
        # Update intensity immediately after event
        lambda_t <- lambda_candidate + ifelse(sign_event > 0, a_pos_lambda, a_neg_lambda)  # Jump in intensity, choose excitation strength based on sign
        varsigma_t <- varsigma_candidate + ifelse(sign_event > 0, a_pos_varsigma, a_neg_varsigma)  # Jump in intensity
      } else {
        t <- t_candidate
      }
    }
    
    
    return_tbl <- bind_rows(return_tbl,
                            tibble(Date=i_day,
                                   SecSinceStart = event_times,
                                   lambda = intensity_record,
                                   varsigma = varsigmas,
                                   LogReturn = LogReturns) %>%
                              mutate(LogPrice = cumsum(LogReturn),
                                     Price = exp(LogPrice))
    )
    
    
    
  }
  
  return(return_tbl)
}



# # Test
# D <- 4
# TT <- 23400
# 
# lambda_det_lin <- seq(0.05, 0.2, length.out=23401)
# lambda_mean_sim <- mean(lambda_det_lin)
# 
# varsigma_det_lin <- seq(0.0001, 0.00004, length.out=23401)
# varsigma_mean_sim <- mean(varsigma_mean_sim)
# 
# hawkes_sim <- simulate_TTSV_hawkes_fast(days=D,
#                                         TT = TT+1,
#                                         lambda_det = lambda_det_lin,
#                                         varsigma_det = varsigma_det_lin,
#                                         a_pos_lambda = 0.5*lambda_mean_sim,
#                                         a_neg_lambda = 1*lambda_mean_sim, 
#                                         b_lambda = 5*lambda_mean_sim,
#                                         a_pos_varsigma = 0.1*varsigma_mean_sim,
#                                         a_neg_varsigma = 0.2*varsigma_mean_sim,
#                                         b_varsigma = 1,
#                                         rho_intensities=0.3)
# 
# # Log-price plot
# ggplot(hawkes_sim %>% filter(SecSinceStart <= 23400)) +
#   geom_line(aes(x=SecSinceStart, y=LogPrice, col=factor(Date))) +
#   facet_wrap(~Date, scales="free")
# 
# # lambda plot
# ggplot(hawkes_sim %>% filter(SecSinceStart <= 23400)) +
#   geom_line(aes(x=SecSinceStart, y=lambda, col=factor(Date))) +
#   facet_wrap(~Date, scales="free")
# 
# # varsigma plot
# ggplot(hawkes_sim %>% filter(SecSinceStart <= 23400)) +
#   geom_line(aes(x=SecSinceStart, y=varsigma, col=factor(Date))) +
#   facet_wrap(~Date, scales="free")
# 
# # amount of ticks
# hawkes_sim %>% group_by(Date) %>% summarize(n_ticks = n())

