library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)

source("sample_schemes_est.R")
source("simulations/sim_TTSV.R")
source("lambda_est.R")
source("varsigma_est.R")

dir_base <- getwd()

asset_set <- c("AA", "AXP", "BA", "BAC", "CAT",
               "DIS", "GE", "GS", "HD", "HON",
               "HPQ", "IBM", "IP", "JNJ", "JPM",
               "KO", "MCD", "MMM", "MO", "MRK",
               "NKE", "PFE", "PG", "UTX", "VZ",
               "WMT", "XOM")

# Paralell computing
core.max <- 30
cl <- makeCluster(min(parallel::detectCores()-1, core.max, length(asset_set)) )
registerDoParallel(cl)

time_start <- Sys.time()
# Loop over the different assets
df_RV <- foreach(
  i_asset = 1:length(asset_set),
  .combine=rbind,
  .packages=c("tidyverse", "tibble", "lubridate", "padr", "slider"),
  .errorhandling="pass"
)%dopar%{
  
  source("sample_schemes_est.R")
  source("simulations/sim_TTSV.R")
  source("lambda_est.R")
  source("varsigma_est.R")
  
  M_set <- c(1,6,13,26,39,78,130,260,390,585,780)
  TT <- 23400
  
  asset <- asset_set[i_asset]
  
  # Read the resampled intrinsic returns
  df_smpl_raw <- readRDS(file = paste0(dir_base, "/application/data/resampled/", asset ,"_prices_resampled_rolling.rds"))
  
  # Filter days with reduced trading time
  days_reduced_trading <- df_smpl_raw %>% 
    dplyr::filter(sampling=="CTS") %>% 
    group_by(Date) %>% 
    summarize(SecSinceStart_max = max(SecSinceStart)) %>% 
    dplyr::filter(SecSinceStart_max <= 21600) %>%  # Less than 6h of trading
    pull(Date)
  
  df_smpl <- df_smpl_raw %>% 
    dplyr::filter(!Date %in% days_reduced_trading)
  
  
  df_RV <- tibble()
  for (M in M_set){
    n_aggregate <- TT/(10*M)
    df_tmp <- df_smpl %>%
      drop_na(return) %>%
      group_by(Date, sampling, days_past) %>%
      summarize(Date=Date,
                sampling=sampling,
                days_past=days_past,
                time_sampling=time_sampling + seconds(10*(n_aggregate-1)),
                return = slider::slide_sum(x=return,
                                           before=0,
                                           after=n_aggregate-1,
                                           step=n_aggregate)) %>%
      drop_na(return)
    
    df_tmp2 <- bind_rows(
      df_tmp %>%
        group_by(Date, sampling, days_past) %>%
        summarize(RV = sum(return^2, na.rm=TRUE),
                  M = M,
                  type_estimator = "RV"),
      df_tmp %>%
        group_by(Date, sampling, days_past) %>%
        summarize(RV = sum(return^2, na.rm=TRUE) + 
                    M/(M-1)*sum(return*lag(return), na.rm=TRUE) + 
                    M/(M-1)*sum(return*lead(return), na.rm=TRUE),
                  M = M,
                  type_estimator = "RV_AC1"))
    
    df_RV <- bind_rows(df_RV, df_tmp2)
  }
  
  df_RV  %>%
    mutate(asset=asset)
  
} 
stopCluster(cl)

time_end <- Sys.time()
(run_time <- time_end - time_start)


saveRDS(df_RV, file = paste0(dir_base, "/application/data/RV_est_RollingSS.rds"))


