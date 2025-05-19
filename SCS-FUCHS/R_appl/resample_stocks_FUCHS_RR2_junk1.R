# args <- commandArgs()
# asset <- args[6]
# print(paste0("Resampling for Asset number ",asset_num))

# Load packages and functions
library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(slider)
library(padr)
library(stringr)


source("/home/fuchs/agmisc/dimitriadis/RVTTSV/R_appl/sample_schemes_est.R")
source("/home/fuchs/agmisc/dimitriadis/RVTTSV/R_appl/lambda_est.R")
source("/home/fuchs/agmisc/dimitriadis/RVTTSV/R_appl/varsigma_est.R")

dir_base <- getwd()
time_start <- Sys.time()

# Set options
start_date_Sample <- "2011-09-01"
days_roll <- c(50)

asset_set <- c("AA", "AXP", "BA", "BAC", "CAT", "DIS", "GE")

numCores <- 7
registerDoParallel(numCores)
foreach (asset = asset_set) %dopar% {
  # Load raw asset ticks
  dat <- readRDS(file = paste0("/home/fuchs/agmisc/dimitriadis/RVTTSV/data/filtered_rds/",asset,"_ticks.rds"))
  
  # "past" estimation sample
  dat_Sample <- dat %>%
    dplyr::filter(Date >= start_date_Sample) %>%
    distinct() # This removes possible duplicated data points
  
  # Apply the sampling schemes (=! HTS) on the simulated returns
  df_smpl <- resample_prices(dat_Sample,
                             sampling_schemes = c("CTS",
                                                  "TTS_rolling", "TTS_realized", "TTS_realized_stopping",
                                                  "BTS_rolling", "BTS_realized_rolling", "BTS_realized_stopping_rolling"),
                             days_rolling=days_roll) %>%
    group_by(Date, sampling, days_avg) %>%
    mutate(return = log(Price) - lag(log(Price)))
  
  
  # Apply the HTS sampling scheme
  max_returns_HTS <- 5000
  # delta_set <- 10^seq(-3.5,-2, by=0.1) # sample even more often!?!?!? with that choice, 1.5 days at the moment! Look at line below next weekend...
  delta_set <- 10^seq(-3.8,-2.2, by=0.05) # Maybe like this?
  df_resample_HTS <- resample_prices_HTS(dat_Sample,
                                         delta_set=delta_set, 
                                         max_returns=max_returns_HTS) 
  
  
  # Safe the resampled prices file
  saveRDS(df_smpl, file = paste0("/home/fuchs/agmisc/dimitriadis/RVTTSV/data/resampled_RR2/", asset ,"_prices_resampled_starting2012.rds"))
  saveRDS(df_resample_HTS, file = paste0("/home/fuchs/agmisc/dimitriadis/RVTTSV/data/resampled_RR2/", asset ,"_prices_HTS_resampled_starting2012.rds"))
  
  
  time_end <- Sys.time()
  (run_time <- time_end - time_start)
}

stopImplicitCluster()



