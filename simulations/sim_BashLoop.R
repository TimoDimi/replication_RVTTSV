args <- commandArgs()
arg_numeric <- as.numeric(args[6])

# Set settings
i_process_setting <- (arg_numeric %% 8) + 1


# Load packages and functions
library(tidyverse)
library(lubridate)
library(padr)
library(slider)

source("sample_schemes_est.R")
source("simulations/sim_TTSV.R")
source("lambda_est.R")
source("varsigma_est.R")
source("RV_PAVG.R")


### Simulation Setup
D <- 250 
days_roll <- 50

# Options
TT <- 23400
M_set <- c(13,15,18,20,26,30,36,39,45,52,60,65,78,90,117,130,156,180,195,234,260,390,468,585,780,1170,2340,4680)

# Deterministic trends: estimated and averaged IBM data
IBM_intensities_avg <- readRDS(file = "simulations/IBM_intensities_est.rds")

LamSig_det <-  IBM_intensities_avg %>%
  dplyr::filter(tau!=23400) %>%
  reframe(time = 0:TT,
          lambda = approx(x=tau, y=lambda_avg, xout=0:TT, rule=2)$y,
          varsigma = approx(x=tau, y=varsigma_avg, xout=0:TT, rule=2)$y)


process_settings_list <- list(c("independent","no"),
                              c("independent","iid"),
                              c("independent","ARMA"),
                              c("independent","ARMA-diurnal"),
                              c("leverage","no"),
                              c("leverage","iid"),
                              c("leverage","ARMA"),
                              c("leverage","ARMA-diurnal"))


process_setting <- process_settings_list[[i_process_setting]]
leverage_setting <- process_setting[1]
noise_setting <- process_setting[2]
sd_eps <- 1.2*10^(-4) # Fix standard deviation of noise process here!

print(paste0("Setting number ", i_process_setting, " which is ", leverage_setting, " and ", noise_setting))

### Simulate TTSV model around the average estimated IBM intensities
# Either with independent or leverage setting
if (leverage_setting == "independent"){
  dat_TTSV_sim <- sim_TTSV(days=D,
                           lambda_det=LamSig_det$lambda, 
                           varsigma_det=LamSig_det$varsigma, 
                           TT=TT+1)
} else {
  dat_TTSV_sim <- sim_TTSV_leverage(days=D,
                                    lambda_det=LamSig_det$lambda, 
                                    varsigma_det=LamSig_det$varsigma, 
                                    TT=TT+1)
}


# Add noise to the true prices
dat_TTSV_sim$prices <- dat_TTSV_sim$prices %>% 
  group_by(Date) %>%
  mutate(eps_noise = noise_process(length(Price), sd_eps=sd_eps, noise_setting=noise_setting),
         LogPrice = LogPrice + eps_noise,
         Price = exp(LogPrice))

# Calculate the true IV
df_IV <- dat_TTSV_sim$intensities %>%
  group_by(Date) %>%
  summarise(IV = sum(lambda*varsigma^2))


# Apply the sampling schemes on the simulated returns
df_resample <- resample_prices(dat_TTSV_sim$prices, days_rolling=days_roll) %>%
  dplyr::filter(Date > max(days_roll)) %>% # Cut off first days_roll days
  group_by(Date, sampling) %>%
  mutate(return = log(Price) - lag(log(Price)),
         i_process_setting=i_process_setting,
         noise_setting=noise_setting,
         leverage_setting=leverage_setting)


# Aggregate returns to higher frequencies and compute returns
df_RV <- tibble()
for (M in M_set){
  n_aggregate <- TT/(5*M) # How many 5-second returns are aggregated
  
  df_tmp <- df_resample %>%
    drop_na(return) %>% # Only remove NAs in the return columns
    group_by(Date, sampling) %>%
    summarize(Date=Date,
              sampling=sampling,
              days_roll=days_avg,
              time_sampling=time_sampling + seconds((5*n_aggregate-1)),
              return = slider::slide_sum(x=return,
                                         before=0,
                                         after=n_aggregate-1,
                                         step=n_aggregate)) %>%
    drop_na(return)
  
  # Compute the RV and RV_PAVG estimators
  df_RV <- bind_rows(df_RV,
                     df_tmp %>%
                       group_by(Date, sampling) %>%
                       summarize(RV = sum(return^2, na.rm=TRUE),
                                 M = M,
                                 type_estimator = "RV"),
                     df_tmp %>%
                       group_by(Date, sampling) %>%
                       summarize(RV = RV_PAVG(return),
                                 M = M,
                                 type_estimator = "RV_PAVG"))
  
  # Free up some memory:
  rm(df_tmp)
  gc()
}

df_est <- left_join(df_RV, df_IV, by=c("Date")) %>%
  mutate(i_process_setting=i_process_setting,
         noise_setting=noise_setting,
         leverage_setting=leverage_setting)

saveRDS(df_est, file = paste0("simulations/data/sim_files_20231125/sim_RVest_file", i_process_setting, "_Time", as.numeric(Sys.time()),".rds"))
