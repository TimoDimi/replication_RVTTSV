library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(slider)
library(scales)

source("sample_schemes_est.R")
source("lambda_est.R")
source("varsigma_est.R")


dir_base <- getwd()
asset <- "IBM"

# Load raw asset ticks
dat <- readRDS(file = paste0(dir_base,"/application/data/filtered_rds/",asset,"_ticks.rds"))


grid <- seq(0, secs_trading, by=30)
secs_trading <- 23400
h <- 2000
m_factor <- 3000 
H <- 40

# Estimate daily intensity
df_intensity_est <- dat %>% 
  dplyr::filter(Date >= as_date("2018-01-01"),
                Date <= as_date("2018-12-31")) %>%
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
            sigma2_hat = lambda_hat*varsigma2_hat) 

# Average over all days in 2018
df_intensity_avg <- df_intensity_est %>% 
  group_by(tau) %>%
  summarize(lambda_avg = mean(lambda_hat),
            varsigma_avg = mean(sqrt(varsigma2_hat)),
            varsigma2_avg = mean(varsigma2_hat),
            sigma2_avg = mean(lambda_hat*varsigma2_hat))

# Safe estimated file
saveRDS(df_intensity_avg, file = "simulations/IBM_intensities_est.rds")



