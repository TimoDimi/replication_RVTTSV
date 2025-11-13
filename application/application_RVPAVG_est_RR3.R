
# This file estimates the pre-averaging RV based on all tick-level data and all assets

# Load packages and functions
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
TT <- 23400
start_date_Sample <- "2012-01-01"

asset_set <- c("AA",  "AXP", "BA",  "BAC", "CAT",
               "DIS", "GE",  "GS",  "HD",  "HON",
               "HPQ", "IBM", "IP",  "JNJ", "JPM",
               "KO",  "MCD", "MMM", "MO",  "MRK",
               "NKE", "PFE", "PG",  "UTX", "VZ",
               "WMT", "XOM")
  
df_RV_PAVG <- tibble()
for (asset in asset_set){
  
  # Read the filtered returns
  df_filtered <- readRDS(file = paste0(dir_base, "/application/data/filtered_rds/", asset ,"_ticks.rds")) %>%
    dplyr::filter(Date >= start_date_Sample)
  
  # Compute RV PAVG estimator
  df_RV_PAVG_asset <- df_filtered %>% 
    group_by(Date) %>%
    mutate(return = c(0, diff(LogPrice))) %>%
    filter(return != 0) %>%
    summarize(RV = RV_PAVG(return)) %>%
    ungroup() %>%
    mutate(asset = asset,
           type_estimator = "RV_PAVG")
  
  # Bind together
  df_RV_PAVG <- bind_rows(df_RV_PAVG, df_RV_PAVG_asset)
}

saveRDS(df_RV_PAVG, file = paste0(dir_base, "/application/data/RV_PAVG_est_starting2012.rds"))




