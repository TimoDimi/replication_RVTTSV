


### Hitting-time based resampling
resample_prices_HTS_randomized <- function(df_prices, delta_set, max_returns=1000, sample_before_prob=1){
  
  prices_HTS_resampled <- tibble()
  
  for (days in unique(df_prices$Date)){
    
    prices_day <- df_prices %>% filter(Date == days)
    return_counter_prev <- 0
    
    for (delta in sort(delta_set, decreasing=T)){
      prices_HTS_resampled_delta <- tibble()
      
      if (return_counter_prev >= max_returns-1) break # Breaks for loop as soon as max_returns are reached for a smaller delta!
      
      row_return_new <- prices_day[1,]
      return_counter <- 0
      
      # Loop while we are not at the end of the day, and below a "max counter" to avoid infinite looping
      while(is.na(row_return_new$SecSinceStart) == FALSE & return_counter < max_returns-1) {
        return_counter <- return_counter + 1
        
        # Filter for all ticks after the current sampling point
        prices_day_truncated <- prices_day %>% 
          filter(SecSinceStart >= row_return_new$SecSinceStart) 
        
        # Sample after or just before we exceed delta 
        df_HitFlags <- prices_day_truncated %>%
          mutate(return_accum = LogPrice - LogPrice[1],
                 return_accum_exceed = (abs(return_accum) >= delta) ) 
        
        row_return_new <- {
          # Select first hit
          first_true <- which(df_HitFlags$return_accum_exceed)[1]
          
          # If there is no "first_true", select a row full of NAs
          if (is.na(first_true)) {
            # No TRUE found → return tibble with NAs
            df_HitFlags[1, ] %>% mutate(across(everything(), ~NA))
          } else {
            rows_to_choose <- df_HitFlags %>% slice(max(first_true - 1, 1):first_true)
            
            if (rows_to_choose$SecSinceStart[1] == prices_day_truncated$SecSinceStart[1]){
              rows_to_choose %>% slice(2)
            } else {
              rows_to_choose %>% 
                slice_sample(n = 1, weight_by = c(sample_before_prob, 1-sample_before_prob))
            }
          }
        } %>% 
          mutate(return_number=return_counter, sampling = "HTS_avg", delta = delta) 

        # Add the new "row" to the data frame (If in the end, no further observation is found by HTS, row_return_new will be an NA row, which we delete again by the drop_na())
        prices_HTS_resampled_delta <- bind_rows(prices_HTS_resampled_delta, row_return_new %>% drop_na(SecSinceStart))
      }
      
      # Add last observation on any day for the remaining return
      if (dim(prices_HTS_resampled_delta)[1] > 0){
        prices_HTS_resampled_delta <- bind_rows(prices_HTS_resampled_delta,
                                                tail(prices_day,1) %>%
                                                  mutate(return_accum=LogPrice-tail(prices_HTS_resampled_delta,1)$LogPrice, 
                                                         return_number=return_counter, return_accum_exceed=F, sampling = "HTS_avg", delta = delta) ) 
      }
      
      # Bind sampled prices to data.frame
      prices_HTS_resampled <- bind_rows(prices_HTS_resampled, prices_HTS_resampled_delta)
      
      return_counter_prev <- return_counter
    }
  }
  
  # Add/delete all (in)sufficient information
  prices_HTS_resampled <- prices_HTS_resampled %>%
    group_by(Date, delta) %>%
    mutate(M_individual=n()) %>%
    ungroup() %>%
    # group_by(delta) %>%
    # mutate(M_avg = mean(M_individual)) %>%
    # ungroup() %>%
    mutate(time_last_tick = as_date(Date) + seconds(SecSinceStart),
           time_sampling=NA,
           days_avg=NA) %>%
    dplyr::select(Date, time_sampling, time_last_tick, SecSinceStart, Price, LogPrice, return_accum, sampling, delta, days_avg, M_individual) %>%
    rename(return=return_accum)
  
  return(prices_HTS_resampled)
}





### Hitting-time based resampling
resample_prices_HTS_grid <- function(df_prices, delta_set, max_returns=1000){
  
  prices_HTS_resampled <- tibble()
  
  for (days in unique(df_prices$Date)){
    
    prices_day <- df_prices %>% filter(Date == days)
    return_counter_prev <- 0
    
    for (delta in sort(delta_set, decreasing=T)){
      prices_HTS_resampled_delta <- tibble()
      
      if (return_counter_prev >= max_returns-1) break # Breaks for loop as soon as max_returns are reached for a smaller delta!
      
      row_return_new <- prices_day[1,]
      return_counter <- 0
      
      # Initial (artificial grid) price to compare in the while loop
      initial_price <- prices_day$LogPrice[1]
      
      # Loop while we are not at the end of the day, and below a "max counter" to avoid infinite looping
      while(is.na(row_return_new$SecSinceStart) == FALSE & return_counter < max_returns-1) {
        return_counter <- return_counter + 1
        
        # Filter for all ticks after the current sampling point
        prices_day_truncated <- prices_day %>% 
          filter(SecSinceStart >= row_return_new$SecSinceStart) 
        
        # Sample whenever we exceed delta 
        row_return_new <- prices_day_truncated %>%
          mutate(return_accum = LogPrice - initial_price,
                 return_accum_exceed = (abs(return_accum) >= delta) ) %>%
          filter(return_accum_exceed == TRUE) %>%
          first() %>% 
          mutate(return_number=return_counter, sampling = "HTS_grid", delta = delta) 
        
        # Update initial price to move either delta up or down.
        initial_price <- initial_price + ifelse(row_return_new$LogPrice > initial_price, 1, -1) * delta
        
        # Add the new "row" to the data frame (If in the end, no further observation is found by HTS, row_return_new will be an NA row, which we delete again by the drop_na())
        prices_HTS_resampled_delta <- bind_rows(prices_HTS_resampled_delta, row_return_new %>% drop_na(SecSinceStart))
      }
      
      # Add last observation on any day for the remaining return
      if (dim(prices_HTS_resampled_delta)[1] > 0){
        prices_HTS_resampled_delta <- bind_rows(prices_HTS_resampled_delta,
                                                tail(prices_day,1) %>%
                                                  mutate(return_accum=LogPrice-tail(prices_HTS_resampled_delta,1)$LogPrice, 
                                                         return_number=return_counter, return_accum_exceed=F, sampling = "HTS_grid", delta = delta) ) 
      }
      
      # Bind sampled prices to data.frame
      prices_HTS_resampled <- bind_rows(prices_HTS_resampled, prices_HTS_resampled_delta)
      
      return_counter_prev <- return_counter
    }
  }
  
  # For the grid sampling, the return_accum is NOT the final return, but the difference between the newly sampled price, and the last tick grid!!!
  
  # Add/delete all (in)sufficient information
  prices_HTS_resampled <- prices_HTS_resampled %>%
    group_by(Date, delta) %>%
    mutate(M_individual=n(),
           return=c(first(return_accum), diff(LogPrice))) %>%
    ungroup() %>%
    # group_by(delta) %>%
    # mutate(M_avg = mean(M_individual)) %>%
    # ungroup() %>%
    mutate(time_last_tick = as_date(Date) + seconds(SecSinceStart),
           time_sampling=NA,
           days_avg=NA) %>%
    dplyr::select(Date, time_sampling, time_last_tick, SecSinceStart, Price, LogPrice, return, return_accum, sampling, delta, days_avg, M_individual)
  
  return(prices_HTS_resampled)
}



### Hitting-time based resampling
resample_prices_HTS_closest <- function(df_prices, delta_set, max_returns=1000){
  
  prices_HTS_resampled <- tibble()
  
  for (days in unique(df_prices$Date)){
    
    prices_day <- df_prices %>% filter(Date == days)
    return_counter_prev <- 0
    
    for (delta in sort(delta_set, decreasing=T)){
      prices_HTS_resampled_delta <- tibble()
      
      if (return_counter_prev >= max_returns-1) break # Breaks for loop as soon as max_returns are reached for a smaller delta!
      
      row_return_new <- prices_day[1,]
      return_counter <- 0
      
      # Loop while we are not at the end of the day, and below a "max counter" to avoid infinite looping
      while(is.na(row_return_new$SecSinceStart) == FALSE & return_counter < max_returns-1) {
        return_counter <- return_counter + 1
        
        # Filter for all ticks after the current sampling point
        prices_day_truncated <- prices_day %>% 
          filter(SecSinceStart >= row_return_new$SecSinceStart) 
        
        # Sample after or just before we exceed delta 
        df_HitFlags <- prices_day_truncated %>%
          mutate(return_accum = LogPrice - LogPrice[1],
                 return_accum_exceed = (abs(return_accum) >= delta) ) 
        
        row_return_new <- {
          # Select first hit
          first_true <- which(df_HitFlags$return_accum_exceed)[1]
          
          # If there is no "first_true", select a row full of NAs
          if (is.na(first_true)) {
            # No TRUE found → return tibble with NAs
            df_HitFlags[1, ] %>% mutate(across(everything(), ~NA))
          } else {
            rows_to_choose <- df_HitFlags %>% slice(max(first_true - 1, 1):first_true)
            
            # Check if the hit was achieved on the next tick already, then use that one (as "before" would get stuck)
            if (rows_to_choose$SecSinceStart[1] == prices_day_truncated$SecSinceStart[1]){
              rows_to_choose %>% slice(2)
            } else {
              # Otherwise, take the closer return to delta
              after_smaller_indicator <- (abs( abs(rows_to_choose$return_accum[2]) - delta) < abs( abs(rows_to_choose$return_accum[1]) - delta))
              
              if(after_smaller_indicator){
                rows_to_choose %>% slice(2)
              } else {
                rows_to_choose %>% slice(1)
              }
            }
          }
        } %>% 
          mutate(return_number=return_counter, sampling = "HTS_closest", delta = delta) 
        
        # Add the new "row" to the data frame (If in the end, no further observation is found by HTS, row_return_new will be an NA row, which we delete again by the drop_na())
        prices_HTS_resampled_delta <- bind_rows(prices_HTS_resampled_delta, row_return_new %>% drop_na(SecSinceStart))
      }
      
      # Add last observation on any day for the remaining return
      if (dim(prices_HTS_resampled_delta)[1] > 0){
        prices_HTS_resampled_delta <- bind_rows(prices_HTS_resampled_delta,
                                                tail(prices_day,1) %>%
                                                  mutate(return_accum=LogPrice-tail(prices_HTS_resampled_delta,1)$LogPrice, 
                                                         return_number=return_counter, return_accum_exceed=F, sampling = "HTS_closest", delta = delta) ) 
      }
      
      # Bind sampled prices to data.frame
      prices_HTS_resampled <- bind_rows(prices_HTS_resampled, prices_HTS_resampled_delta)
      
      return_counter_prev <- return_counter
    }
  }
  
  # Add/delete all (in)sufficient information
  prices_HTS_resampled <- prices_HTS_resampled %>%
    group_by(Date, delta) %>%
    mutate(M_individual=n()) %>%
    ungroup() %>%
    # group_by(delta) %>%
    # mutate(M_avg = mean(M_individual)) %>%
    # ungroup() %>%
    mutate(time_last_tick = as_date(Date) + seconds(SecSinceStart),
           time_sampling=NA,
           days_avg=NA) %>%
    dplyr::select(Date, time_sampling, time_last_tick, SecSinceStart, Price, LogPrice, return_accum, sampling, delta, days_avg, M_individual) %>%
    rename(return=return_accum)
  
  return(prices_HTS_resampled)
}