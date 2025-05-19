args <- commandArgs()
asset <- args[6]
# junk <- args[7]
# 
# print(paste0("Computing RV for junk ", junk," for the Asset ", asset))

# junk <- 1

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
M_set <- c(1,13,26,39,78,130,260,390,780,1170,1560,2340,4680) 
n_aggregate_stopping_set <- c(1,2,3,4,5,6,8,10,15,20,30,40,60,80,100,150,200,300,400)

TT <- 23400
start_date_Sample <- "2012-01-01"
# date_split_junks <- "2016-01-01"
# date_split_junks <- "2013-01-01"

# Read the resampled intrinsic returns
df_smpl_raw_nonHTS <- readRDS(file = paste0(dir_base, "/application/data/resampled_RR2/", asset ,"_prices_resampled_starting2012.rds"))
df_smpl_raw_HTS <- readRDS(file = paste0(dir_base, "/application/data/resampled_RR2/", asset ,"_prices_HTS_resampled_starting2012.rds"))

df_smpl_raw <- bind_rows(df_smpl_raw_nonHTS, df_smpl_raw_HTS) %>%
  dplyr::filter(Date >= start_date_Sample)

# if (junk == 1){
#   df_smpl_raw <- df_smpl_raw %>%
#     dplyr::filter(Date < date_split_junks & Date >= start_date_Sample)
# } else {
#   df_smpl_raw <- df_smpl_raw %>%
#     dplyr::filter(Date >= date_split_junks & Date >= start_date_Sample) 
# }


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
    filter(!str_detect(sampling, "stopping") & sampling!="HTS") %>%
    drop_na(return) %>%
    group_by(Date, sampling, days_avg) %>%
    reframe(Date=Date,
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

# Aggregate returns to higher frequencies and compute returns for THE STOPPING TIME TTS AND RBTS
df_RV_stopping <- tibble()
for (n_aggregate in n_aggregate_stopping_set){
  
  # Aggregate returns to higher frequencies 
  df_tmp_stopping <- df_smpl %>%
    filter(str_detect(sampling, "stopping")) %>%
    drop_na(return) %>% # Only remove NAs in the return columns
    group_by(Date, sampling) %>%
    reframe(Date=Date,
            sampling=sampling,
            days_roll=days_avg,
            time_sampling=time_sampling, # not exact, but does not matter in the end!
            return = slider::slide_sum(x=return, 
                                       before=0,
                                       after=n_aggregate-1,
                                       step=n_aggregate)) %>%
    drop_na(return) %>%
    group_by(Date, sampling) %>%
    mutate(M=n())
  
  # Compute the RV and RV_PAVG estimators
  df_RV_stopping <- bind_rows(df_RV_stopping,
                              df_tmp_stopping %>%
                                group_by(Date, sampling) %>%
                                summarize(RV = sum(return^2, na.rm=TRUE),
                                          M = first(M),
                                          n_aggregate=n_aggregate,
                                          type_estimator = "RV"),
                              df_tmp_stopping %>%
                                group_by(Date, sampling) %>%
                                summarize(RV = RV_PAVG(return),
                                          M = first(M),
                                          n_aggregate=n_aggregate,
                                          type_estimator = "RV_PAVG"))
  
  # Free up some memory:
  rm(df_tmp_stopping)
  gc()
}

# Compute the RV and RV_PAVG estimators
df_RV_HTS <- bind_rows(df_smpl %>%
                         filter(sampling=="HTS") %>%
                         drop_na(return) %>% # Only remove NAs in the return columns
                         group_by(Date, sampling, delta) %>%
                         summarize(RV = sum(return^2, na.rm=TRUE),
                                   M = first(M_individual),
                                   type_estimator = "RV"),
                       df_smpl %>%
                         filter(sampling=="HTS") %>%
                         drop_na(return) %>% # Only remove NAs in the return columns
                         group_by(Date, sampling, delta) %>%
                         summarize(RV = RV_PAVG(return),
                                   M = first(M_individual),
                                   type_estimator = "RV_PAVG"))



# Join data frames
df_RV_joint <- bind_rows(df_RV, df_RV_HTS, df_RV_stopping) %>%
  mutate(asset=asset)

time_end <- Sys.time()
(run_time <- time_end - time_start)


saveRDS(df_RV_joint, file = paste0(dir_base, "/application/data/RV_est_RR2/RV_est_starting2012", asset, ".rds"))


