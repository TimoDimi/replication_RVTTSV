library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)

source("sample_schemes_est.R")
source("simulations/sim_TTSV.R")
source("lambda_est.R")
source("varsigma_est.R")
source("RV_comparison.R")


dir_base <- getwd()

asset_set <- c("AA", "AXP", "BA", "BAC", "CAT",
               "DIS", "GE", "GS", "HD", "HON",
               "HPQ", "IBM", "IP", "JNJ", "JPM",
               "KO", "MCD", "MMM", "MO", "MRK",
               "NKE", "PFE", "PG", "UTX", "VZ",
               "WMT", "XOM")


for (i_asset in 1:length(asset_set)){
  asset <- asset_set[i_asset]
  
  # Read filtered but raw data files
  dat_raw <- tibble()
  for (year in 2001:2019){
    for (month in 1:12){
      file_path <- paste0(dir_base, "/application/data/raw/", asset, "/", year,"/",month,"/", asset, "_trades_",year,"_",month,"_filtered.txt")
      if (file.exists(file_path)){
        dat_raw <- bind_rows(dat_raw, read_delim(file = file_path))
      }
    }
  }
  
  # Convert variable names
  Secs_At_Start <- 34200 
  dat <- dat_raw %>%
    mutate(Date = ymd(Date),
           DateTime = Date + seconds(Time),
           SecSinceStart = Time - Secs_At_Start,
           LogPrice=log(Price),
           Asset=asset) %>%
    select(Date, DateTime, SecSinceStart, Price, LogPrice, Asset)
  
  # Safe estimated file
  saveRDS(dat, file = paste0(dir_base,"/application/data/filtered_rds/",asset,"_ticks.rds"))
}

