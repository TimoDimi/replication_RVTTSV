args <- commandArgs()
print(args)

arg_numeric <- as.numeric(args[6])
set.seed(arg_numeric)

# Load packages and functions
# install.packages("lubridate", "padr", "slider", "MASS", "tibble", "scales")
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(padr)
library(slider)
library(stringr)
library(tibble)

base_dir <- "/home/fuchs/agmisc/dimitriadis/RVTTSV/R_sim/"
save_dir <- "/home/fuchs/agmisc/dimitriadis/RVTTSV/data/simulation/data_rds_20250509/"

source(paste0(base_dir, "sample_schemes_est.R"))
source(paste0(base_dir, "sim_TTSV.R"))
source(paste0(base_dir, "HawkesSim.R"))
source(paste0(base_dir, "lambda_est.R"))
source(paste0(base_dir, "varsigma_est.R"))
source(paste0(base_dir, "RV_PAVG.R"))


# Settings for different amount of ticks, noise setting, and leverage settings
lambda_settings_hlp <- c(2000, 8000, 32000)
leverage_settings_hlp <- c("independent", "Hawkes")
noise_setting_hlp <- c("iid", "ARMA", "ARMA-diurnal")

process_setting_tbl_full <- expand.grid(list(lambda_setting=lambda_settings_hlp, leverage_setting=leverage_settings_hlp, noise_setting=noise_setting_hlp)) %>% 
  as_tibble()

# Manual reduction!
process_setting_tbl <- process_setting_tbl_full[c(4,5,6,2,8,11,14,17),]

length_settings <- dim(process_setting_tbl)[1]
i_process_setting <- (arg_numeric %% length_settings) + 1

leverage_setting <- process_setting_tbl[[i_process_setting, "leverage_setting"]]
noise_setting <- process_setting_tbl[[i_process_setting, "noise_setting"]]
lambda_setting <- process_setting_tbl[[i_process_setting, "lambda_setting"]]



### Simulation Setup for days, frequencies, sampling schemes, etc
D <- 150
days_roll <- 50

# Options
TT <- 23400

# Different standard deviations of the noise
sd_eps_factors <- c(0, 0.25, 0.5, 1, 2)

M_set <- c(13,15,18,20,26,30,36,39,45,52,60,65,78,90,117,130,156,180,195,234,260,390,468,585,780,1170,1560,2340)

# n_aggregation set for the stopping time sampling schemes
n_aggregate_stopping_set <- c(1,2,4,6,8,10,20,30,40,50,60,80,100,120,150,200,250,300,400)

# Reduced M_set for HTS deltas
M_set_reduced <- c(13,18,26,39,78,130,180,260,390,585,780,1170,1560,2340)

max_returns_HTS <- 5000
M_set_HTS <- sort(c(min(M_set_reduced)*c(0.3,0.5,0.8), M_set_reduced))
                  #,  max(M_set)*c(1.5))) 


# Set sampling schemes
sampling_schemes_endofday_set = c("CTS", 
                                  "TTS_true",
                                  "TTS_rolling",
                                  "TTS_realized", 
                                  "BTS_true",
                                  "BTS_rolling",
                                  "BTS_realized_true",
                                  "BTS_realized_rolling")
sampling_schemes_stopping_set <- c("TTS_realized_stopping", "BTS_realized_stopping_rolling")
sampling_schemes_set <- c(sampling_schemes_endofday_set, sampling_schemes_stopping_set)



# Deterministic trends: estimated and averaged IBM data
IBM_intensities_avg <- readRDS(file = paste0(base_dir, "IBM_intensities_est.rds"))

LamSig_det_raw <-  IBM_intensities_avg %>%
  dplyr::filter(tau!=23400) %>%
  reframe(time = 0:TT,
          lambda = approx(x=tau, y=lambda_avg, xout=0:TT, rule=2)$y,
          varsigma = approx(x=tau, y=varsigma_avg, xout=0:TT, rule=2)$y)

# Different standard deviations of the noise
sd_eps_set <- sd_eps_factors * mean(LamSig_det_raw$varsigma)


# Hawkes type factors
a_pos_lambda_factor <- 0.05
a_neg_lambda_factor <- 0.1
b_lambda_factor <- 0.25
eta <- mean(c(a_pos_lambda_factor, a_neg_lambda_factor)) / b_lambda_factor

c_Hawkes_varsigma <- case_when(
  lambda_setting == 2000 ~ 1.045,
  lambda_setting == 8000 ~ 1,
  lambda_setting == 32000 ~ 0.785,
)

# Weigh lambda by how many ticks we want to have (approximately) per day in the end
Expected_ticks_raw_Hawkes <- 1/(1-eta) * sum(LamSig_det_raw$lambda)
LamSig_det_Hawkes <- LamSig_det_raw %>% mutate(lambda = lambda * lambda_setting/Expected_ticks_raw_Hawkes,
                                               varsigma = varsigma * sqrt(c_Hawkes_varsigma) / sqrt(lambda_setting/Expected_ticks_raw_Hawkes/(1-eta)) )

Expected_ticks_raw_independent <- sum(LamSig_det_raw$lambda)
LamSig_det_independent <- LamSig_det_raw %>% mutate(lambda = lambda * lambda_setting/Expected_ticks_raw_independent,
                                                    varsigma = varsigma / sqrt(lambda_setting/Expected_ticks_raw_independent) )


if (leverage_setting == "independent"){
  dat_TTSV_sim <- sim_TTSV(days=D,
                           lambda_det=LamSig_det_independent$lambda,  
                           varsigma_det=LamSig_det_independent$varsigma, 
                           TT=TT+1)
  
  df_prices <- dat_TTSV_sim$prices
  df_intensities <- dat_TTSV_sim$intensities
} else {
  lambda_mean_sim <- mean(LamSig_det_Hawkes$lambda)
  varsigma_mean_sim <- mean(LamSig_det_Hawkes$varsigma)
  
  dat_TTSV_Hawkes_sim <- simulate_TTSV_Hawkes(days=D,
                                              TT = TT+1,
                                              lambda_det = LamSig_det_Hawkes$lambda, 
                                              varsigma_det = LamSig_det_Hawkes$varsigma, # Account for the Hawkes type effect in varsigma?
                                              a_pos_lambda = a_pos_lambda_factor*lambda_mean_sim,
                                              a_neg_lambda = a_neg_lambda_factor*lambda_mean_sim, 
                                              b_lambda = b_lambda_factor*lambda_mean_sim,
                                              a_pos_varsigma = 0*varsigma_mean_sim,
                                              a_neg_varsigma = 0.1*varsigma_mean_sim,
                                              b_varsigma = 0.5,
                                              rho_intensities=0.3)
  
  # Obtain price and intensity DFs
  df_prices <- dat_TTSV_Hawkes_sim
  
  # Any problems for Hawkes? We seem to have a small bias right now in the MSE estimates!!!
  df_intensities <- dat_TTSV_Hawkes_sim %>%
    group_by(Date) %>%
    reframe(SecSinceStart_linear = 0:23400,
            lambda_approx = approx(x=SecSinceStart, y=lambda, xout=SecSinceStart_linear, rule=2)$y,
            varsigma_approx = approx(x=SecSinceStart, y=varsigma, xout=SecSinceStart_linear, rule=2)$y) %>%
    rename(SecSinceStart=SecSinceStart_linear, lambda=lambda_approx, varsigma=varsigma_approx)
}


# Calculate the true IV
df_IV <- full_join(df_intensities %>%
                     group_by(Date) %>%
                     summarise(IV = sum(lambda*varsigma^2)),
                   df_prices %>%
                     group_by(Date) %>%
                     summarise(rIV = sum(varsigma^2)),
                   by="Date")



# Approximate delta set such that we have M_set samples per day!
IV_det_sim <- LamSig_det_independent %>% summarize(IV=sum(lambda*varsigma^2)) %>% pull(IV)
delta_set <- sqrt(IV_det_sim/M_set_HTS)
                


start_time <- Sys.time()
df_est <- tibble()
# Loop over the noise intensities
for (sd_eps_loop in sd_eps_set){
  
  # Add noise to the true prices
  prices_df_noise <- df_prices %>% 
    group_by(Date) %>%
    mutate(eps_noise = noise_process(length(Price), sd_eps=sd_eps_loop, noise_setting=noise_setting),
           LogPrice = LogPrice + eps_noise,
           Price = exp(LogPrice))
  

  # Apply the sampling schemes on the simulated returns
  df_resample <- resample_prices(prices_df_noise, days_rolling=days_roll, sampling_schemes=sampling_schemes_set) %>%
    dplyr::filter(Date > max(days_roll)) %>% # Cut off first days_roll days
    group_by(Date, sampling) %>%
    mutate(return = LogPrice - lag(LogPrice),
           i_process_setting=i_process_setting,
           lambda_setting=lambda_setting,
           noise_setting=noise_setting,
           leverage_setting=leverage_setting,
           sd_eps=sd_eps_loop)

  
  # HTS based resampling!!!
  df_resample_HTS <- resample_prices_HTS(prices_df_noise %>% dplyr::filter(Date > days_roll),  # Cut off first days_roll days
                                         delta_set=delta_set, 
                                         max_returns=max_returns_HTS) %>%
    group_by(Date, sampling, delta) %>%
    mutate(M_expected = IV_det_sim/delta^2,
           i_process_setting=i_process_setting,
           lambda_setting=lambda_setting,
           noise_setting=noise_setting,
           leverage_setting=leverage_setting,
           sd_eps=sd_eps_loop)
  
  # Aggregate returns to higher frequencies and compute returns for all sampling schems BUT HTS (and the stopping time based ones)
  df_RV <- tibble()
  for (M in M_set){
    n_aggregate <- TT/(5*M) # How many 5-second returns are aggregated
    
    # Aggregate returns to higher frequencies 
    df_tmp <- df_resample %>%
      filter(!str_detect(sampling, "stopping") & sampling!="HTS") %>%
      drop_na(return) %>% # Only remove NAs in the return columns
      group_by(Date, sampling) %>%
      reframe(Date=Date,
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
  

  
  # Aggregate returns to higher frequencies and compute returns for THE STOPPING TIME TTS AND RBTS
  df_RV_stopping <- tibble()
  for (n_aggregate in n_aggregate_stopping_set){

    # Aggregate returns to higher frequencies 
    df_tmp_stopping <- df_resample %>%
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
  df_RV_HTS <- bind_rows(df_resample_HTS %>%
                           drop_na(return) %>% # Only remove NAs in the return columns
                           group_by(Date, sampling, delta) %>%
                           summarize(RV = sum(return^2, na.rm=TRUE),
                                     M = first(M_individual),
                                     M_expected = first(M_expected),
                                     type_estimator = "RV"),
                         df_resample_HTS %>%
                           drop_na(return) %>% # Only remove NAs in the return columns
                           group_by(Date, sampling, delta) %>%
                           summarize(RV = RV_PAVG(return),
                                     M = first(M_individual),
                                     M_expected = first(M_expected),
                                     type_estimator = "RV_PAVG"))
  
  
  # Join data frames
  df_RV_joint <- bind_rows(df_RV, df_RV_HTS, df_RV_stopping)
  
  
  df_est <- bind_rows(df_est, 
                      left_join(df_RV_joint, df_IV, by=c("Date")) %>%
                        mutate(i_process_setting=i_process_setting,
                               lambda_setting=lambda_setting,
                               noise_setting=noise_setting,
                               leverage_setting=leverage_setting,
                               sd_eps=sd_eps_loop)
  )
}
  
end_time <- Sys.time()
(run_time <- end_time - start_time)

saveRDS(df_est, file = paste0(save_dir,"/sim_RVest_file_", i_process_setting, "_Time", as.numeric(Sys.time()),".rds"))


