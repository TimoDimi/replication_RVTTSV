
resample_by_intensity <- function(df){

  dates_set <- unique(df$Date)
  
  df_resampled <- tibble()
  for (index_date in 1:length(dates_set)){
    date <- dates_set[index_date]
    
    df_date <- df %>% dplyr::filter(Date==date)
    
    # Do the real resampling
    df_tmp <- df_date %>%
      mutate(Intensity_dt = as_date(date) + seconds(Intensity)) %>%
      padr::thicken(interval = "5 sec", 
                    colname="Intensity_rounded", 
                    rounding="up",
                    by="Intensity_dt") %>%
      group_by(Intensity_rounded) %>%
      summarise(Price=last(Price),
                SecSinceStart=last(SecSinceStart)) %>%
      ungroup() %>%
      padr::pad(start_val = as_date(date) + seconds(5),
                end_val = as_date(date) + seconds(23400)) %>%
      tidyr::fill(c("SecSinceStart", "Price"), .direction="down") %>%
      rename(time_sampling = Intensity_rounded) %>%
      mutate(time_last_tick = as_date(date) + seconds(SecSinceStart),
             Date=date) %>%
      dplyr::select(Date, time_sampling, time_last_tick, SecSinceStart, Price)
    
    # Use the very first observation as opening price mapped to the opening hour
    df_FirstObs <-  head(df_date,1) %>%
      mutate(time_sampling = as_date(date) + seconds(0),
             time_last_tick =  as_date(date) + seconds(SecSinceStart)) %>%
      dplyr::select(Date, time_sampling, time_last_tick, SecSinceStart, Price)
    
    # Bind with previous days, and attach the very first observation as opening price mapped to the opening hour
    df_tmp2 <- bind_rows(df_FirstObs, df_tmp) %>%
      tidyr::fill(c("time_last_tick", "SecSinceStart", "Price"), .direction="down")
    
    df_resampled <- bind_rows(df_resampled, df_tmp2)
  }
  
  
  
  df_resampled <- df_resampled %>%
    mutate(LogPrice = log(Price))
  
  return(df_resampled)
}




### General resampling
resample_prices <- function(df_prices, 
                            sampling_schemes = c("CTS", 
                                                 "TTS_true", "TTS_daily", "TTS_rolling", "TTS_realized", "TTS_realized_stopping",
                                                 "BTS_true", "BTS_realized_true", "BTS_daily", "BTS_rolling", "BTS_realized_rolling", "BTS_realized_stopping_rolling"),   
                            days_rolling = 2,
                            secs_trading=23400, 
                            h=2000, m_factor=2000, H=40){
  

  # Estimate intensities
  grid <- seq(0, secs_trading, by=30)
  
  ### True intensities on a grid (only compute if required!)
  true_sampling_schemes <- c("TTS_true", "BTS_true", "BTS_realized_true")
  tryCatch(
    if (any(true_sampling_schemes %in% sampling_schemes)){
      df_intensity_true_grid <- df_prices %>% 
        group_by(Date) %>%
        mutate(sigma2 = lambda*varsigma^2) %>%
        reframe(tau = grid,
                lambda = approx(x=SecSinceStart, 
                                y=lambda, 
                                xout=tau,
                                rule=2)$y,
                sigma2 = approx(x=SecSinceStart, 
                                y=sigma2, 
                                xout=tau,
                                rule=2)$y,
                Lambda = (cumsum(lambda)-first(lambda))*secs_trading/(sum(lambda)-first(lambda)),
                Sigma2 = (cumsum(sigma2)-first(sigma2))*secs_trading/(sum(sigma2)-first(sigma2))) %>%
        rename(SecSinceStart=tau)
    }
  )
  
  ### Estimated intensities on a grid
  df_intensity_est_grid <- df_prices %>% 
    group_by(Date) %>%
    reframe(tau = grid,
            lambda_hat = lambda_est(tau_grid=grid, 
                                    t_tick=SecSinceStart,
                                    TT=secs_trading, 
                                    k=k_Epanechnikov, 
                                    h=h),
            varsigma2_hat = varsigma2_est(tau_grid = grid,
                                          t_tick = SecSinceStart, 
                                          prices_tick = LogPrice, 
                                          TT=secs_trading, 
                                          k=k_Epanechnikov, 
                                          H=H,
                                          m_factor=m_factor),
            sigma2_hat = lambda_hat*varsigma2_hat,
            Lambda_hat = (cumsum(lambda_hat)-first(lambda_hat))*secs_trading/(sum(lambda_hat)-first(lambda_hat)),
            Varsigma2_hat = (cumsum(varsigma2_hat)-first(varsigma2_hat))*secs_trading/(sum(varsigma2_hat)-first(varsigma2_hat)),
            Sigma2_hat = (cumsum(sigma2_hat)-first(sigma2_hat))*secs_trading/(sum(sigma2_hat)-first(sigma2_hat))) %>%
    rename(SecSinceStart=tau)
  
  
  df_prices_resampled <- tibble()
  
  ### CTS
  tryCatch(
    if ("CTS" %in% sampling_schemes){
      df_prices_resampled <- bind_rows(
        df_prices_resampled,
        df_prices %>% 
          group_by(Date) %>%
          mutate(Intensity = SecSinceStart) %>%
          ungroup() %>%
          resample_by_intensity() %>%
          mutate(sampling="CTS"))
    }
  )
  
  
  ### TTS true
  tryCatch(
    if ("TTS_true" %in% sampling_schemes){
      df_prices_resampled <- bind_rows(
        df_prices_resampled,
        df_intensity_true_grid %>%
          full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
          arrange(Date, SecSinceStart) %>%
          mutate(Intensity=approx(x=SecSinceStart, 
                                  y=Lambda, 
                                  xout=SecSinceStart,
                                  rule=2)$y) %>%
          dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
          na.omit() %>%
          resample_by_intensity() %>%
          mutate(sampling="TTS_true")
      )
    }
  )
  
  ### BTS true
  tryCatch(
    if ("BTS_true" %in% sampling_schemes){
      df_prices_resampled <- bind_rows(
        df_prices_resampled,
        df_intensity_true_grid %>%
          full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
          arrange(Date, SecSinceStart) %>%
          mutate(Intensity=approx(x=SecSinceStart, 
                                  y=Sigma2, 
                                  xout=SecSinceStart,
                                  rule=2)$y) %>%
          dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
          na.omit() %>%
          resample_by_intensity() %>%
          mutate(sampling="BTS_true")
      )
    }
  )
  
  
  ### TTS rolling
  tryCatch(
    if ("TTS_rolling" %in% sampling_schemes){
      
      for (days_avg in days_rolling){
        first_full_day <- df_prices$Date %>% unique() %>% .[max(days_rolling)+1]
        
        # Compute rolling mean over Lambda intensities
        df_intensity_rollmean <- df_intensity_est_grid %>%
          group_by(SecSinceStart) %>%
          arrange(Date) %>%
          mutate(Intensity = slider::slide_dbl(Lambda_hat, mean, .before = days_avg, .after = -1)) %>%
          ungroup()
        
        # Merge with df_prices, interpolate on ticks, and resample prices
        df_prices_resampled <- bind_rows(
          df_prices_resampled,
          df_intensity_rollmean %>%
            full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
            filter(Date >= first_full_day) %>%
            arrange(Date, SecSinceStart) %>%
            group_by(Date) %>%
            mutate(Intensity=approx(x=SecSinceStart,  # THIS IS CORRECT!
                                    y=Intensity, 
                                    xout=SecSinceStart,
                                    rule=2)$y) %>%
            dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
            drop_na(LogPrice) %>%
            resample_by_intensity() %>%
            mutate(sampling=paste0("TTS_rolling_avg",days_avg),
                   days_avg=days_avg)
        )
      }
    }
  )
  
  
  ### BTS rolling
  tryCatch(
    if ("BTS_rolling" %in% sampling_schemes){
      
      for (days_avg in days_rolling){
        first_full_day <- df_prices$Date %>% unique() %>% .[max(days_rolling)+1]
        
        # Compute rolling mean over Lambda intensities
        df_intensity_rollmean <- df_intensity_est_grid %>%
          group_by(SecSinceStart) %>%
          arrange(Date) %>%
          mutate(Intensity = slider::slide_dbl(Sigma2_hat, mean, .before = days_avg, .after = -1)) %>%
          ungroup()
        
        # Merge with df_prices, interpolate on ticks, and resample prices
        df_prices_resampled <- bind_rows(
          df_prices_resampled,
          df_intensity_rollmean %>%
            full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
            filter(Date >= first_full_day) %>%
            arrange(Date, SecSinceStart) %>%
            group_by(Date) %>%
            mutate(Intensity=approx(x=SecSinceStart,  # THIS IS CORRECT!
                                    y=Intensity, 
                                    xout=SecSinceStart,
                                    rule=2)$y) %>%
            dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
            drop_na(LogPrice) %>%
            resample_by_intensity() %>%
            mutate(sampling=paste0("BTS_rolling_avg",days_avg),
                   days_avg=days_avg)
        )
      }
    }
  )
  
  ### TTS realized
  tryCatch(
    if ("TTS_realized" %in% sampling_schemes){
      df_prices_resampled <- bind_rows(
        df_prices_resampled,
        df_prices %>% 
          group_by(Date) %>%
          mutate(Intensity = row_number()*secs_trading/n()) %>%
          resample_by_intensity()  %>%
          mutate(sampling="TTS_realized")
      )
    }
  )
  
  
    
  ### BTS realized true
  tryCatch(
    if ("BTS_realized_true" %in% sampling_schemes){
      df_prices_resampled <- bind_rows(
        df_prices_resampled,
        df_prices %>%
          arrange(Date, SecSinceStart) %>%
          group_by(Date) %>%
          mutate(intensity = varsigma^2 * 1, # each tick gets it's estimated tick variance as intensity increase
                 Intensity = (cumsum(intensity)-first(intensity))*secs_trading/(sum(intensity)-first(intensity))) %>%
          dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
          drop_na(LogPrice) %>%
          resample_by_intensity()  %>%
          mutate(sampling="BTS_realized_true")
      )
    }
  )
  
  
  ### BTS realized rolling
  tryCatch(
    if ("BTS_realized_rolling" %in% sampling_schemes){
      for (days_avg in days_rolling){
        first_full_day <- df_prices$Date %>% unique() %>% .[max(days_rolling)+1]
        
        # Compute rolling mean over Lambda intensities
        df_intensity_rollmean <- df_intensity_est_grid %>%
          group_by(SecSinceStart) %>%
          arrange(Date) %>%
          mutate(varsigma2_hat = slider::slide_dbl(varsigma2_hat, mean, .before = days_avg, .after = -1)) %>%
          dplyr::select(Date, SecSinceStart, varsigma2_hat) %>%
          ungroup()
        
        # Merge with df_prices, interpolate on ticks, and resample prices
        df_prices_resampled <- bind_rows(
          df_prices_resampled,
          df_intensity_rollmean %>%
            full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
            filter(Date >= first_full_day) %>%
            arrange(Date, SecSinceStart) %>%
            group_by(Date) %>%
            mutate(varsigma2_hat=approx(x=SecSinceStart,  # THIS IS CORRECT!
                                        y=varsigma2_hat, 
                                        xout=SecSinceStart,
                                        rule=2)$y,
                   intensity = varsigma2_hat * 1, # each tick gets it's estimated tick variance as intensity increase
                   Intensity = (cumsum(intensity)-first(intensity))*secs_trading/(sum(intensity)-first(intensity))) %>%
            dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
            drop_na(LogPrice) %>%
            resample_by_intensity() %>%
            mutate(sampling=paste0("BTS_realized_rolling_avg",days_avg),
                   days_avg=days_avg)
        )
      }
    }  
  )
  
  
  
  # #############################################################################  
  # NEW stopping time rTTS and rBTS sampling schemes
  tryCatch(
    if ("TTS_realized_stopping" %in% sampling_schemes){
      df_prices_resampled <- bind_rows(
        df_prices_resampled,
        df_prices %>%
          group_by(Date) %>%
          mutate(ticks_day = n()) %>%
          slice(unique(c(seq(1, first(ticks_day), by=5), ticks_day))) %>%
          mutate(M = n(),
                 time_last_tick = as_date(Date) + seconds(SecSinceStart),
                 time_sampling = as_date(Date) + seconds((row_number()-1)*23400/(M-1))) %>%
          dplyr::select(Date, time_sampling, time_last_tick, SecSinceStart, Price, LogPrice, M, ticks_day) %>%
          mutate(sampling="TTS_realized_stopping")
      )
    }
  )
  
  
  ### BTS realized stopping!!! rolling
  tryCatch(
    if ("BTS_realized_stopping_rolling" %in% sampling_schemes){
      for (days_avg in days_rolling){
        first_full_day <- df_prices$Date %>% unique() %>% .[max(days_rolling)+1]
        
        # Compute rolling mean over Lambda intensities
        df_intensity_rollmean <- df_intensity_est_grid %>%
          group_by(SecSinceStart) %>%
          arrange(Date) %>%
          mutate(varsigma2_hat = slider::slide_dbl(varsigma2_hat, mean, .before = days_avg, .after = -1)) %>%
          dplyr::select(Date, SecSinceStart, varsigma2_hat) %>%
          ungroup()
        
        df_prices_intensity_day <- df_intensity_rollmean %>%
          full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
          filter(Date >= first_full_day) %>%
          arrange(Date, SecSinceStart) %>%
          group_by(Date) %>%
          mutate(varsigma2_hat=approx(x=SecSinceStart,  # THIS IS CORRECT!
                                      y=varsigma2_hat, 
                                      xout=SecSinceStart,
                                      rule=2)$y,
                 intensity = varsigma2_hat * 1, # each tick gets it's estimated tick variance as intensity increase
                 Intensity = (cumsum(intensity)-first(intensity))*secs_trading/(sum(intensity)-first(intensity))) %>%
          dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
          drop_na(LogPrice) %>% 
          mutate(ticks_day = n())
        
        
        # Resample here
        dates_set <- unique(df_prices_intensity_day$Date)
  
        df_resampled <- tibble()
        for (index_date in 1:length(dates_set)){
          date <- dates_set[index_date]
          
          # df_date <- df_prices_intensity_day %>% dplyr::filter(Date==date)
        df_hlp <- df_prices_intensity_day %>% dplyr::filter(Date==date) %>% 
          dplyr::filter(Date==date)  %>%
          mutate(sample_every_Intensity = 5*23400/(ticks_day),
                 integer_sampling = Intensity %/% sample_every_Intensity) %>%
          ungroup()
        

        # Resample (approximately!!!) every 5 tricks but according to rIV!
        df_tmp3 <- full_join(df_hlp, tibble(Date=date, integer_sampling = 0:(max(df_hlp$integer_sampling)+1)), by=c("Date", "integer_sampling")) %>%
          arrange(Date, integer_sampling, SecSinceStart) %>%
          tidyr::fill(c("SecSinceStart", "Price", "Intensity", "LogPrice", "ticks_day", "sample_every_Intensity"), .direction="down") %>%
          group_by(integer_sampling) %>%
          slice_head(n=1) %>%
          ungroup() %>%
          tidyr::fill(c("SecSinceStart", "Price"), .direction="down") %>%
          mutate(time_sampling = as_date(date) + seconds(cumsum(sample_every_Intensity)),
                 time_last_tick = as_date(date) + seconds(SecSinceStart),
                 Date=date) %>%
          dplyr::select(Date, time_sampling, time_last_tick, SecSinceStart, Price, LogPrice, ticks_day, sample_every_Intensity)
        
        
        df_resampled <- bind_rows(df_resampled, df_tmp3)
      }
        
        
        
        # Merge with df_prices, interpolate on ticks, and resample prices
        df_prices_resampled <- bind_rows(
          df_prices_resampled,
          df_resampled %>%
            mutate(sampling=paste0("BTS_realized_stopping_rolling_avg",days_avg),
                   days_avg=days_avg)
        )
      }
    }  
  )
  
  


  
  
    
  ### ### ### The following "daily" sampling schemes are not considered in the paper anymore (December 2023)

  # TTS daily (estimated intensities!)
  tryCatch(
    if ("TTS_daily" %in% sampling_schemes){
      df_prices_resampled <- bind_rows(
        df_prices_resampled,
        df_intensity_est_grid %>%
          full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
          arrange(Date, SecSinceStart) %>%
          group_by(Date) %>%
          mutate(Intensity=approx(x=SecSinceStart,  # THIS IS INDEED CORRECT!
                                  y=Lambda_hat, 
                                  xout=SecSinceStart,
                                  rule=2)$y) %>%
          dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
          na.omit() %>%
          resample_by_intensity() %>%
          mutate(sampling="TTS_daily")
      )
    }
  )
  
  
  # BTS daily (estimated intensities!)
  tryCatch(
    if ("BTS_daily" %in% sampling_schemes){
      df_prices_resampled <- bind_rows(
        df_prices_resampled,
        df_intensity_est_grid %>%
          full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
          arrange(Date, SecSinceStart) %>%
          group_by(Date) %>%
          mutate(Intensity=approx(x=SecSinceStart,  # THIS IS INDEED CORRECT!
                                  y=Sigma2_hat, 
                                  xout=SecSinceStart,
                                  rule=2)$y) %>%
          dplyr::select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
          na.omit() %>%
          resample_by_intensity() %>%
          mutate(sampling="BTS_daily")
      )
    }
  )
  
  return(df_prices_resampled)
}



### Hitting-time based resampling
resample_prices_HTS <- function(df_prices, delta_set, max_returns=1000){
  
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
        
        # Sample whenever we exceed delta 
        row_return_new <- prices_day_truncated %>%
          mutate(return_accum = LogPrice - LogPrice[1],
                 return_accum_exceed = (abs(return_accum) >= delta) ) %>%
          filter(return_accum_exceed == TRUE) %>%
          first() %>% 
          mutate(return_number=return_counter, sampling = "HTS", delta = delta) 
        
        # Add the new "row" to the data frame (If in the end, no further observation is found by HTS, row_return_new will be an NA row, which we delete again by the drop_na())
        prices_HTS_resampled_delta <- bind_rows(prices_HTS_resampled_delta, row_return_new %>% drop_na(SecSinceStart))
      }
      
      # Add last observation on any day for the remaining return
      if (dim(prices_HTS_resampled_delta)[1] > 0){
        prices_HTS_resampled_delta <- bind_rows(prices_HTS_resampled_delta,
                                                tail(prices_day,1) %>%
                                                  mutate(return_accum=LogPrice-tail(prices_HTS_resampled_delta,1)$LogPrice, 
                                                         return_number=return_counter, return_accum_exceed=F, sampling = "HTS", delta = delta) ) 
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
