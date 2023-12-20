args <- commandArgs()
asset <- args[6]
junk <- args[7]

print(paste0("Computing RV for junk ", junk," for the Asset ", asset))


# Load packaes and functions
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(slider)
library(padr)

source("sample_schemes_est.R")
source("simulations/sim_TTSV.R")
source("lambda_est.R")
source("varsigma_est.R")
source("RV_PAVG.R")

dir_base <- getwd()
time_start <- Sys.time()

# Set options
M_set <- c(1,13,26,39,78,260,390,780,2340,4680)
TT <- 23400
start_date_Sample <- "2012-01-01"
date_split_junks <- "2016-01-01"


# Read the resampled intrinsic returns
df_smpl_raw <- readRDS(file = paste0(dir_base, "/application/data/resampled_RR/", asset ,"_prices_resampled_starting2012.rds"))

if (junk == 1){
  df_smpl_raw <- df_smpl_raw %>%
    dplyr::filter(Date < date_split_junks & Date >= start_date_Sample)
} else {
  df_smpl_raw <- df_smpl_raw %>%
    dplyr::filter(Date >= date_split_junks & Date >= start_date_Sample) 
}


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
  n_aggregate <- TT/(5*M)
  df_tmp <- df_smpl %>%
    drop_na(return) %>%
    group_by(Date, sampling, days_avg) %>%
    summarize(Date=Date,
              sampling=sampling,
              days_avg=days_avg,
              time_sampling=time_sampling + seconds(5*(n_aggregate-1)),
              return = slider::slide_sum(x=return,
                                         before=0,
                                         after=n_aggregate-1,
                                         step=n_aggregate)) %>%
    drop_na(return)
  
  # Compute the RV and RV_PAVG estimators
  df_RV <- bind_rows(df_RV,
                     df_tmp %>%
                       group_by(Date, sampling, days_avg) %>%
                       summarize(RV = sum(return^2, na.rm=TRUE),
                                 M = M,
                                 type_estimator = "RV"),
                     df_tmp %>%
                       group_by(Date, sampling, days_avg) %>%
                       summarize(RV = RV_PAVG(return),
                                 M = M,
                                 type_estimator = "RV_PAVG"))
  
  
}

df_RV <- df_RV %>%
  mutate(asset=asset)

time_end <- Sys.time()
(run_time <- time_end - time_start)


saveRDS(df_RV, file = paste0(dir_base, "/application/data/RV_est/RV_est_starting2012", asset, "junk_", junk, ".rds"))


