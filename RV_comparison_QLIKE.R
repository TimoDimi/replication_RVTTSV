
# This function implements the data-based RV ranking method of Patton (2011, JoE) for the QLIKE loss function
RV_comparison_QLIKE <- function(RV, RV_baseline, IV_proxy, B=500){
  
  QLIKE_fct <- function(x,y){ y/x - log(y/x) - 1 }
  
  loss_diff <- na.omit(QLIKE_fct(RV_baseline,IV_proxy) - QLIKE_fct(RV, IV_proxy))
  Delta_L <- mean(loss_diff, na.rm = T)
  
  # Relative (R)MSE improvements
  QLIKE_rel <- (mean(QLIKE_fct(RV_baseline,IV_proxy), na.rm=T) - mean(QLIKE_fct(RV,IV_proxy), na.rm=T)) / mean(QLIKE_fct(RV_baseline,IV_proxy), na.rm=T)

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
                QLIKE_rel=QLIKE_rel,
                p_val = p_val))
}
