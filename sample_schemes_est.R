
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
      select(Date, time_sampling, time_last_tick, SecSinceStart, Price)
    
    # Use the very first observation as opening price mapped to the opening hour
    df_FirstObs <-  head(df_date,1) %>%
      mutate(time_sampling = as_date(date) + seconds(0),
             time_last_tick =  as_date(date) + seconds(SecSinceStart)) %>%
      select(Date, time_sampling, time_last_tick, SecSinceStart, Price)
    
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
                                                 "TTS_true", "TTS_daily", "TTS_rolling", "TTS_realized",
                                                 "BTS_true", "BTS_realized_true", "BTS_daily", "BTS_rolling", "BTS_realized_rolling"),   
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
          select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
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
          select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
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
            select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
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
            select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
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
          select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
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
          select(Date, SecSinceStart, varsigma2_hat) %>%
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
            select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
            drop_na(LogPrice) %>%
            resample_by_intensity() %>%
            mutate(sampling=paste0("BTS_realized_rolling_avg",days_avg),
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
          select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
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
          select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
          na.omit() %>%
          resample_by_intensity() %>%
          mutate(sampling="BTS_daily")
      )
    }
  )
  
  return(df_prices_resampled)
}


