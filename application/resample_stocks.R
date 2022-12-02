library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(doParallel)


source("sample_schemes_est.R")
source("lambda_est.R")
source("varsigma_est.R")


# Put this on a cluster: This is computationally heavy. E.g., each loop requires approximately 10GB memory
dir_base <- getwd()

asset_set <- c("AA", "AXP", "BA", "BAC", "CAT",
               "DIS", "GE", "GS", "HD", "HON",
               "HPQ", "IBM", "IP", "JNJ", "JPM",
               "KO", "MCD", "MMM", "MO", "MRK",
               "NKE", "PFE", "PG", "UTX", "VZ",
               "WMT", "XOM")

# Paralell computing
core.max <- 14
cl <- makeCluster(min(parallel::detectCores()-1, core.max, length(asset_set)) )
registerDoParallel(cl)

time_start <- Sys.time()
# Loop over different assets in paralell
res_df <- foreach(
  i_asset = 1:length(asset_set),
  .combine=rbind,
  .packages=c("tidyverse", "tibble", "lubridate", "padr", "slider"),
  .errorhandling="stop"
  )%dopar%{
    
  source("sample_schemes_est.R")
  source("lambda_est.R")
  source("varsigma_est.R")
  
  asset <- asset_set[i_asset]
  
  start_date_Sample <- "2005-01-01"
  days_RollMean <- c(1,5,20,250)
  
  # Load raw asset ticks
  dat <- readRDS(file = paste0(dir_base,"/application/data/filtered_rds/",asset,"_ticks.rds"))
                 
  # "past" estimation sample
  dat_Sample <- dat %>% dplyr::filter(Date >= start_date_Sample)
  
  # Apply the different sampling schemes
  df_smpl <- bind_rows(CTS(dat_Sample) %>% 
                         mutate(sampling="CTS"),
                       TTS_daily(dat_Sample) %>% 
                         mutate(sampling="TTS_daily"),
                       BTS_daily(dat_Sample, h=2000, m_factor=2000, H=40) %>% 
                         mutate(sampling="BTS_daily"),
                       TTS_rolling_days(dat_Sample, days_RollMean=days_RollMean) %>% 
                         mutate(sampling="TTS_rolling"),
                       BTS_rolling_days(dat_Sample, days_RollMean=days_RollMean, h=2000, m_factor=2000, H=40) %>% 
                         mutate(sampling="BTS_rolling")) %>%
    group_by(Date, sampling, days_past) %>%
    mutate(return = LogPrice - lag(LogPrice))
  
  # Safe the resampled prices file
  saveRDS(df_smpl, file = paste0(dir_base, "/application/data/resampled/", asset ,"_prices_resampled_rolling.rds"))
}
stopCluster(cl)

time_end <- Sys.time()
(run_time <- time_end - time_start)


 
