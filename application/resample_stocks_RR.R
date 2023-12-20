args <- commandArgs()
asset <- args[6]
print(paste0("Resampling for Asset ",asset))


# Load packages and functions
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(slider)

source("sample_schemes_est.R")
source("lambda_est.R")
source("varsigma_est.R")

dir_base <- getwd()
time_start <- Sys.time()


# Set options
start_date_Sample <- "2011-09-01"
days_roll <- c(50)


# Load raw asset ticks
dat <- readRDS(file = paste0(dir_base,"/application/data/filtered_rds/",asset,"_ticks.rds"))

# "past" estimation sample
dat_Sample <- dat %>%
  dplyr::filter(Date >= start_date_Sample) %>%
  distinct() # This removes possible duplicated data points

# Apply the sampling schemes on the simulated returns
df_smpl <- resample_prices(dat_Sample,
                           sampling_schemes = c("CTS",
                                                "TTS_rolling", "TTS_realized",
                                                "BTS_rolling", "BTS_realized_rolling"),
                           days_rolling=days_roll) %>%
  group_by(Date, sampling, days_avg) %>%
  mutate(return = log(Price) - lag(log(Price)))


# Safe the resampled prices file
saveRDS(df_smpl, file = paste0(dir_base, "/application/data/resampled_RR/", asset ,"_prices_resampled_starting2012.rds"))


time_end <- Sys.time()
(run_time <- time_end - time_start)


