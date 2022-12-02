
resample_by_intensity <- function(df){

  dates_set <- unique(df$Date)
  
  df_resampled <- tibble()
  for (index_date in 1:length(dates_set)){
    date <- dates_set[index_date]
    
    df_date <- df %>% dplyr::filter(Date==date)
    
    # Do the real resampling
    df_tmp <- df_date %>%
      mutate(Intensity_dt = as_date(date) + seconds(Intensity)) %>%
      padr::thicken(interval = "10 sec", 
                    colname="Intensity_rounded", 
                    rounding="up",
                    by="Intensity_dt") %>%
      group_by(Intensity_rounded) %>%
      summarise(Price=last(Price),
                SecSinceStart=last(SecSinceStart)) %>%
      ungroup() %>%
      padr::pad(start_val = as_date(date) + seconds(10),
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



CTS <- function(df_prices, secs_trading=23400){
  df <- df_prices %>% 
    group_by(Date) %>%
    mutate(Intensity = SecSinceStart) %>%
    ungroup() %>%
    resample_by_intensity()
  df
}



TTS_true <- function(df_prices, secs_trading=23400){
  grid <- seq(0, secs_trading, by=30)
  
  # Intensity must be summed on an equidistant(!) grid
  df_intensity_true <- df_prices %>% 
    group_by(Date) %>%
    summarize(tau = grid,
              lambda = approx(x=SecSinceStart, 
                              y=lambda, 
                              xout=tau,
                              rule=2)$y,
              Intensity = (cumsum(lambda)-first(lambda))*secs_trading/(sum(lambda)-first(lambda))) %>%
    rename(SecSinceStart=tau)
  # Comment to previous line: Modify "cumsum" such that the intensity is zero at the first second
  
  
  df <- df_intensity_true %>%
    full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
    arrange(Date, SecSinceStart) %>%
    mutate(Intensity=approx(x=SecSinceStart, 
                            y=Intensity, 
                            xout=SecSinceStart,
                            rule=2)$y) %>%
    select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
    na.omit() %>%
    resample_by_intensity()
  
  return(df)
}


BTS_true <- function(df_prices, secs_trading=23400){
  
  grid <- seq(0, secs_trading, by=30)
  
  df_intensity_true <- df_prices %>% 
    group_by(Date) %>%
    mutate(sigma2 = lambda*varsigma^2) %>%
    summarize(tau = grid,
              sigma2 = approx(x=SecSinceStart, 
                              y=sigma2, 
                              xout=tau,
                              rule=2)$y,
              Intensity = (cumsum(sigma2)-first(sigma2))*secs_trading/(sum(sigma2)-first(sigma2))) %>%
    rename(SecSinceStart=tau)
  # Comment to previous line: Modify "cumsum" such that the intensity is zero at the first second.
  
  
  df <- df_intensity_true %>%
    full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
    arrange(Date, SecSinceStart) %>%
    mutate(Intensity=approx(x=SecSinceStart, 
                            y=Intensity, 
                            xout=SecSinceStart,
                            rule=2)$y) %>%
    select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
    na.omit() %>%
    resample_by_intensity()
  
  return(df)
}



TTS_daily <- function(df_prices, secs_trading=23400){
  df <- df_prices %>% 
    group_by(Date) %>%
    mutate(Intensity = row_number()*secs_trading/n()) %>%
    resample_by_intensity()
  df
}



BTS_daily <- function(df_prices, secs_trading=23400, h=2000, m_factor=2000, H=40){
  
  grid <- seq(0, secs_trading, by=30)
  
  df_intensity_est <- df_prices %>% 
    group_by(Date) %>%
    summarize(tau = grid,
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
              intensity = sigma2_hat,
              Intensity = (cumsum(sigma2_hat)-first(sigma2_hat))*secs_trading/(sum(sigma2_hat)-first(sigma2_hat))) %>%
    rename(SecSinceStart=tau)
  

  df <- df_intensity_est %>%
    full_join(df_prices, by=c("Date", "SecSinceStart")) %>%
    arrange(Date, SecSinceStart) %>%
    mutate(Intensity=approx(x=SecSinceStart,  # THIS IS INDEED CORRECT!
                            y=Intensity, 
                            xout=SecSinceStart,
                            rule=2)$y) %>%
    select(Date, SecSinceStart, Intensity, Price, LogPrice) %>%
    na.omit() %>%
    resample_by_intensity()

  return(df)
}



TTS_past <- function(df_prices, df_est, secs_trading=23400){
  grid <- seq(0, secs_trading, by=30)
  
  # estimate the intensity (amount of ticks) over past trading days at an equidistant grid,
  # and average over all past days in "df_est"
  
  df_intensity_est <- df_est %>% 
    group_by(Date) %>%
    mutate(Intensity = row_number()*secs_trading/n()) %>%
    summarize(tau=grid,
              Intensity = approx(x=SecSinceStart,
                                 y=Intensity,
                                 xout=tau,
                                 rule=2)$y) %>%
    ungroup() %>%
    group_by(tau) %>%
    summarize(Intensity = mean(Intensity))
  
  # For each date in df_prices, approximate the previously estimated intensity 
  # at the observed ticks, and apply the resample_by_intensity() function
  df <- df_prices %>%
    group_by(Date) %>%
    mutate(Intensity = approx(x=df_intensity_est$tau,
                              y=df_intensity_est$Intensity,
                              xout=SecSinceStart,
                              rule=2)$y) %>%
    resample_by_intensity()
  
  return(df)
}




BTS_past <- function(df_prices, df_est, secs_trading=23400, h=1000, m_factor=1000, H=40){
  grid <- seq(0, secs_trading, by=30)
  
  # estimate the intensity (lambda and varsigma2) over past trading days at an equidistant grid,
  # and average over all past days in "df_est"
  
  df_intensity_est <- df_est %>% 
    group_by(Date) %>%
    summarize(tau = grid,
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
              Intensity = (cumsum(sigma2_hat)-first(sigma2_hat))*secs_trading/(sum(sigma2_hat)-first(sigma2_hat))) %>%
    ungroup() %>%
    group_by(tau) %>%
    summarize(Intensity = mean(Intensity))
  
  
  # For each date in df_prices, approximate the previously estimated intensity 
  # at the observed ticks, and apply the resample_by_intensity() function
  df <- df_prices %>%
    group_by(Date) %>%
    mutate(Intensity = approx(x=df_intensity_est$tau,
                              y=df_intensity_est$Intensity,
                              xout=SecSinceStart,
                              rule=2)$y) %>%
    resample_by_intensity()
  
  return(df)
}





TTS_rolling_days <- function(df_prices, days_RollMean=500, secs_trading=23400){
  grid <- seq(0, secs_trading, by=30)
  
  # estimate the intensity (lambda and varsigma2) for all days, and average over past days for resampling
  df_intensity_est_hlp <- df_prices %>% 
    group_by(Date) %>%
    mutate(Intensity = row_number()*secs_trading/n()) %>%
    summarize(tau=grid,
              Intensity = approx(x=SecSinceStart,
                                 y=Intensity,
                                 xout=tau,
                                 rule=2)$y) %>%
    ungroup()
  
  # Determine how many days should be cut in the beginning:
  first_full_day <- df_prices$Date %>% unique() %>% .[max(days_RollMean)+1]
  
  # Allow for a vector of different days_RollMean
  df <- tibble()
  for (days in days_RollMean){
    df_intensity_est <- df_intensity_est_hlp %>%
      group_by(tau) %>%
      arrange(Date) %>%
      mutate(Intensity = slider::slide_dbl(Intensity, mean, .before = days, .after = -1)) %>%
      rename(SecSinceStart=tau) %>%
      ungroup()
    
    # Apply the respective "resample_by_intensity()" function
    df_tmp <- df_intensity_est %>%
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
      mutate(days_past=days)
    
    df <- bind_rows(df, df_tmp)
  }
  
  return(df)
}








BTS_rolling_days <- function(df_prices, days_RollMean=500, secs_trading=23400, h=1000, m_factor=1000, H=40){
  grid <- seq(0, secs_trading, by=30)
  
  # estimate the intensity (lambda and varsigma2) for all days, and average over past days for resampling
  df_intensity_est_hlp <- df_prices %>% 
    group_by(Date) %>%
    summarize(tau = grid,
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
              Intensity = (cumsum(sigma2_hat)-first(sigma2_hat))*secs_trading/(sum(sigma2_hat)-first(sigma2_hat))) %>%
    ungroup() 
  
  
  # Determine how many days should be cut in the beginning:
  first_full_day <- df_prices$Date %>% unique() %>% .[max(days_RollMean)+1]
  
  # Allow for a vector of different days_RollMean
  df <- tibble()
  for (days in days_RollMean){
    df_intensity_est <- df_intensity_est_hlp %>%
      group_by(tau) %>%
      arrange(Date) %>%
      mutate(Intensity = slider::slide_dbl(Intensity, mean, .before = days, .after = -1)) %>%
      rename(SecSinceStart=tau) %>%
      ungroup()
    
    
    # Apply the respective "resample_by_intensity()" function
    df_tmp <- df_intensity_est %>%
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
      mutate(days_past=days)
    
    df <- bind_rows(df, df_tmp)
  }
  
  return(df)
}



