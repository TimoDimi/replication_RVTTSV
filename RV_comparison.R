
# This function implements the data-based RV ranking method of Patton (2011, JoE)
RV_comparison <- function(RV, RV_baseline, IV_proxy, B=500){
  loss_diff <- na.omit((RV_baseline - IV_proxy)^2 - (RV - IV_proxy)^2)
  Delta_L <- mean(loss_diff, na.rm = T)
  
  # Relative (R)MSE improvements
  MSE_rel <- (mean((RV_baseline-IV_proxy)^2, na.rm=T) - mean((RV-IV_proxy)^2, na.rm=T))/mean((RV_baseline-IV_proxy)^2, na.rm=T)
  RMSE_rel <- (sqrt(mean((RV_baseline-IV_proxy)^2, na.rm=T)) - sqrt(mean((RV-IV_proxy)^2, na.rm=T)))/sqrt(mean((RV_baseline-IV_proxy)^2, na.rm=T))

  # Block Bootstrap
  n <- length(loss_diff)
  bl <- ceiling(sqrt(n)) # Default block length
  nb <- ceiling(n/bl) # Amounts of blocks
  blocks_list <- split(loss_diff, ceiling(seq_along(loss_diff)/bl))
  
  Delta_L_B <- rep(NA, B)
  for (b in  1:B){
    # Draw more block index samples as necessary and simply cut the vector at n to guarantee a resampled vector of length n!
    block_index_sample <- sample(1:nb, size=nb+10, replace=TRUE)
    loss_diff_resample <- blocks_list[block_index_sample] %>% 
      unlist() %>% 
      as.numeric() %>%
      na.omit() %>%
      .[1:n]
    Delta_L_B[b] <- mean(loss_diff_resample, na.rm=TRUE)
  }
  
  # Normalized loss difference, this is NOT a valid t-statistic!!!!! 
  if (var(loss_diff) > 0){
    Omega <- length(loss_diff) * sandwich::vcovHAC(lm(loss_diff~1))
    mean_loss_diff_normalized <- sqrt(length(loss_diff)) * Delta_L/sqrt(as.numeric(Omega))
    mean_loss_diff_normalized_bt <- Delta_L/sd(Delta_L_B, na.rm=TRUE)
  } else {
    mean_loss_diff_normalized <- Delta_L
    mean_loss_diff_normalized_bt <- Delta_L
  }
  
  
  # If loss_diff is negative, we search for the fraction of bootstrap samples that are positive, 
  # and vice versa if loss_diff is positive. Hence the multiplication with the sign() function
  # Devide by 2 to get the p-value of a two-sided test.
  p_val <- mean(0 > (sign(Delta_L)*Delta_L_B), na.rm=TRUE)/2
  
  return(tibble(mean_loss_diff = Delta_L,
                mean_loss_diff_normalized=mean_loss_diff_normalized,
                mean_loss_diff_normalized_bt=mean_loss_diff_normalized_bt,
                MSE_rel=MSE_rel,
                RMSE_rel=RMSE_rel,
                p_val = p_val))
}
