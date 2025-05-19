
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

base_dir <- "/Users/timodimitriadis/Documents/Code/replication_RVTTSV/SCS-FUCHS/R_sim/"
# save_dir <- "/home/fuchs/agmisc/dimitriadis/RVTTSV/data/simulation/data_rds/"

source(paste0(base_dir, "sample_schemes_est.R"))
source(paste0(base_dir, "sim_TTSV.R"))
source(paste0(base_dir, "HawkesSim.R"))
source(paste0(base_dir, "lambda_est.R"))
source(paste0(base_dir, "varsigma_est.R"))
source(paste0(base_dir, "RV_PAVG.R"))

i_process_setting <- 1 # Just needed for completeness!!!

leverage_setting <- "independent"
noise_setting <- "iid"
lambda_setting <- 8000


### Simulation Setup for days, frequencies, sampling schemes, etc
D <- 150
days_roll <- 50

sampling_intensity <- c("CTS", "TTS_realized", "BTS_realized_rolling_avg50")

# Options
TT <- 23400

# Different standard deviations of the noise
sd_eps_factors <- c(0.25,1)

M_set <- c(13,15,18,20,26,30,36,39,45,52,60,65,78,90,117,130,156,180,195,234,260,390,468,585,780,1170,2340)

# n_aggregation set for the stopping time sampling schemes
n_aggregate_stopping_set <- c(1,2,4,6,8,10)

# Extend M_set a bit to account for more sampling due to noise, and less due to small(er) lambda
M_set_reduced <- c(13,18,26,39,78,130,180,260,390,585,780,1170,1560,2340)


max_returns_HTS <- 5000
M_set_HTS <- sort(c(min(M_set_reduced)*c(0.3,0.5,0.8), M_set_reduced))
#                    , max(M_set)*c(1.5,2)))


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
IV_det_sim <- LamSig_det_raw %>% summarize(IV=sum(lambda*varsigma^2)) %>% pull(IV)
delta_set <- sqrt(IV_det_sim/M_set_HTS)



start_time <- Sys.time()
prices_df_noise <- tibble()
df_resample_HTS <- tibble()
df_resample_aggr <- tibble()
# Loop over the noise intensities
for (sd_eps_loop in sd_eps_set){
  
  # Add noise to the true prices
  prices_df_noise_tmp <- df_prices %>% 
    group_by(Date) %>%
    mutate(eps_noise = noise_process(length(Price), sd_eps=sd_eps_loop, noise_setting=noise_setting),
           LogPrice_true = LogPrice,
           Price_true = Price,
           LogPrice = LogPrice_true + eps_noise,
           Price = exp(LogPrice),
           sd_eps = sd_eps_loop)
  
  # Merge prices
  prices_df_noise <- bind_rows(prices_df_noise, prices_df_noise_tmp)
  
  
  
  # Apply the sampling schemes on the simulated returns
  df_resample <- resample_prices(prices_df_noise_tmp, days_rolling=days_roll, sampling_schemes=sampling_schemes_set) %>%
    dplyr::filter(Date > max(days_roll)) %>% # Cut off first days_roll days
    group_by(Date, sampling) %>%
    mutate(return = LogPrice - lag(LogPrice),
           i_process_setting=i_process_setting,
           lambda_setting=lambda_setting,
           noise_setting=noise_setting,
           leverage_setting=leverage_setting,
           sd_eps=sd_eps_loop)
  
  # Aggregate intensity based sampling (overcoming some difficulties...)
  for (M_loop in M_set){
    n_aggregate <- TT/(5*M_loop) # How many 5-second returns are aggregated
    
    # Aggregate returns to higher frequencies 
    df_tmp <- df_resample %>%
      dplyr::filter(sampling %in% sampling_intensity) %>% 
      # dplyr::filter(sampling %in% c("CTS")) %>% 
      drop_na(return) %>% # Only remove NAs in the return columns
      group_by(Date, sampling) %>%
      reframe(Date=Date,
              sampling=sampling,
              days_roll=days_avg,
              M=M_loop,
              time_sampling=time_sampling + seconds((5*n_aggregate-5)), # Careful, this is the "intrinsic time"!!!
              # Following lines: Obtain the last tick before sampling time
              time_last_tick=slide_vec(
                .x = time_last_tick,
                .f = last,      # Extract the last date in the window
                .before = 0,
                .after = n_aggregate - 1,
                .step = n_aggregate
              ),
              return = slider::slide_sum(x=return,
                                         before=0,
                                         after=n_aggregate-1,
                                         step=n_aggregate),
      ) %>%
      drop_na(return) %>%
      mutate(SecSinceStart = (hour(ymd_hms(time_last_tick)) * 3600 + minute(ymd_hms(time_last_tick)) * 60 + second(ymd_hms(time_last_tick))),
             sd_eps=sd_eps_loop)
    
    # Merge intensity resampled sampling
    df_resample_aggr <- bind_rows(df_resample_aggr, df_tmp)
  }
  
  
  
  # HTS based resampling!!!
  df_resample_HTS_tmp <- resample_prices_HTS(prices_df_noise_tmp %>% dplyr::filter(Date > days_roll),  # Cut off first days_roll days
                                             delta_set=delta_set, 
                                             max_returns=max_returns_HTS) %>%
    group_by(Date, sampling, delta) %>%
    mutate(M_expected = IV_det_sim/delta^2,
           i_process_setting=i_process_setting,
           lambda_setting=lambda_setting,
           noise_setting=noise_setting,
           leverage_setting=leverage_setting,
           sd_eps=sd_eps_loop)
  
  # Merge HTS sampling
  df_resample_HTS <- bind_rows(df_resample_HTS, df_resample_HTS_tmp)
}





#################  #################  #################  #################
# The following code computes summary statistics over noise-relevant quantities to approximate the bias!

# Merge data.frames  (with rounding!) (The observation at the start of the day is missing here, which is not substantial)
# For HTS
df_joint_HTS <- left_join(df_resample_HTS %>% ungroup() %>% dplyr::select(Date, SecSinceStart, LogPrice, return, sampling, M_individual, delta, sd_eps) %>% mutate(SecSinceStart=round(SecSinceStart,3)), 
                          prices_df_noise %>% ungroup() %>% dplyr::select(Date, SecSinceStart, LogPrice, LogPrice_true, eps_noise, sd_eps) %>% mutate(SecSinceStart=round(SecSinceStart,3)), 
                          by=c("Date", "SecSinceStart", "LogPrice", "sd_eps")) %>%
  dplyr::filter(Date > days_roll)



# For the intensity based sampling schemes (with rounding!)
df_joint_IntSampling <- left_join(df_resample_aggr %>% ungroup() %>% dplyr::select(Date, SecSinceStart, sampling, M, return, sd_eps) %>% mutate(SecSinceStart=round(SecSinceStart,3)), 
                                  prices_df_noise %>% ungroup() %>% dplyr::select(Date, SecSinceStart, LogPrice, LogPrice_true, eps_noise, sd_eps)  %>% mutate(SecSinceStart=round(SecSinceStart,3)), 
                                  by=c("Date", "SecSinceStart", "sd_eps"))



# Include lagged observations (but first one still missing...)
df_joint_withlag <- bind_rows(df_joint_HTS, df_joint_IntSampling) %>%
  group_by(Date, sd_eps, sampling, delta, M) %>%
  mutate(lag_eps_noise = lag(eps_noise),
         lag_LogPrice = lag(LogPrice),
         lag_LogPrice_true = lag(LogPrice_true),
         return_true = LogPrice_true - lag_LogPrice_true) %>%
  ungroup()


# Estimate interesting quantities
df_noise_est <- df_joint_withlag %>%
#   filter(abs(return != 0) %>%
  group_by(sampling, sd_eps, delta, M) %>%
  reframe(M = sum(first(M_individual), first(M), na.rm=T), # Note: Either M OR M_individual is NA!
          sd_eps_true = mean(sd_eps),
          mean_noise = mean(eps_noise, na.rm=T),
          sd_noise = sd(eps_noise, na.rm=T),
          mean_noise_diff = mean(eps_noise - lag_eps_noise, na.rm=T),
          sd_noise_diff = sd(eps_noise - lag_eps_noise, na.rm=T),
          var_noise_diff= sd_noise_diff^2,
          cov_eps_noise_lag_est = cov(eps_noise, lag_eps_noise, use="na.or.complete"),
          cor_eps_noise_lag_est = cor(eps_noise, lag_eps_noise, use="na.or.complete"),
          cor_return_eps_noise = cor(return_true, eps_noise, use="na.or.complete"),
          cov_return_eps_noise_diff = cov(return_true, (eps_noise-lag_eps_noise), use="na.or.complete"),
          cor_return_eps_noise_diff = cor(return_true, (eps_noise-lag_eps_noise), use="na.or.complete")
  ) %>% 
  arrange(M, sampling, sd_eps) 


# Obtain final noise estimates
df_bias_approx <- df_noise_est %>%
  group_by(sampling, sd_eps, delta, M) %>%
  reframe(M = mean(M),
          sd = mean(sd_eps),
          bias_approx_varterm = M*var_noise_diff,
          bias_approx_covterm = 2*M*cov_return_eps_noise_diff,
          bias_approx = bias_approx_varterm + bias_approx_covterm) 

# Save bias approx file
saveRDS(df_bias_approx, file = paste0("simulations/data/bias_approx_20250514.rds"))


ggplot(df_bias_approx) +
  geom_line(aes(x=M, y=bias_approx_varterm, col=sampling), linetype=1) +
  geom_line(aes(x=M, y=bias_approx_covterm, col=sampling), linetype=2) +
  facet_wrap(~sd_eps, scales="free", nrow=1) +
  scale_x_continuous(breaks=c(26,78,390,1560),
                     trans='log2') +
  coord_cartesian(xlim=c(26,1560)) +   
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines"))








#################  #################  #################  #################
### Plot the HTS Overshooting effect

library(patchwork)

delta_value_approx1 <- 0.0033179174
delta_value_approx2 <- 0.0012115321
delta_value_approx3 <- 0.0003831201

# delta 1
df_plot_HTS1 <- df_resample_HTS %>%
  dplyr::filter(sd_eps==0,
                near(delta, delta_value_approx1),
                abs(return) >= delta) %>%
  mutate(delta_fct = factor(delta))

levels(df_plot_HTS1$delta_fct) <- c(expression(paste(delta %~~% 0.0033, ",  ", bar(M)  %~~% 12))) 

bin_seq <- seq(0, 1.2*max(abs(df_plot_HTS1$return)), by=delta_value_approx1/20)
bin_seq <- sort(unique(c(-bin_seq, bin_seq)))

pHTS1 <- ggplot(df_plot_HTS1) +
  geom_histogram(aes(x=return),
                 fill=hue_pal()(3)[1],
                 col="gray50",
                 breaks=bin_seq) +
  geom_hline(yintercept=0, col="gray50") +
  geom_vline(aes(xintercept=delta), col="black") +
  geom_vline(aes(xintercept=-delta), col="black") +
  facet_wrap(~ delta_fct, scales="free", labeller = label_parsed) +
  theme_bw() +
  theme(legend.position="none")

pHTS1

# delta 2
df_plot_HTS2 <- df_resample_HTS %>%
  dplyr::filter(sd_eps==0,
                near(delta, delta_value_approx2),
                abs(return) >= delta) %>%
  mutate(delta_fct = factor(delta))

levels(df_plot_HTS2$delta_fct) <- c(expression(paste(delta %~~% 0.0012, ",  ", bar(M)  %~~% 80))) 

bin_seq <- seq(0, 1.2*max(abs(df_plot_HTS2$return)), by=delta_value_approx2/10)
bin_seq <- sort(unique(c(-bin_seq, bin_seq)))

pHTS2 <- ggplot(df_plot_HTS2) +
  geom_histogram(aes(x=return),
                 fill=hue_pal()(3)[2],
                 col="gray50",
                 breaks=bin_seq) +
  geom_hline(yintercept=0, col="gray50") +
  geom_vline(aes(xintercept=delta), col="black") +
  geom_vline(aes(xintercept=-delta), col="black") +
  facet_wrap(~ delta_fct, scales="free", labeller = label_parsed) +
  theme_bw() +
  theme(legend.position="none")


# delta 3
df_plot_HTS3 <- df_resample_HTS %>%
  dplyr::filter(sd_eps==0,
                near(delta, delta_value_approx3),
                abs(return) >= delta) %>%
  mutate(delta_fct = factor(delta))


levels(df_plot_HTS3$delta_fct) <- c(expression(paste(delta %~~% 0.00038, ",  ", bar(M)  %~~% 720))) 

bin_seq <- seq(0, 1.2*max(abs(df_plot_HTS3$return)), by=delta_value_approx3/5)
bin_seq <- sort(unique(c(-bin_seq, bin_seq)))

pHTS3 <- ggplot(df_plot_HTS3) +
  geom_histogram(aes(x=return), 
                 fill=hue_pal()(3)[3],
                 col="gray50",
                 breaks=bin_seq) +
  geom_hline(yintercept=0, col="gray50") +
  geom_vline(aes(xintercept=delta), col="black") +
  geom_vline(aes(xintercept=-delta), col="black") +
  facet_wrap(~ delta_fct, scales="free", labeller = label_parsed) +
  theme_bw() +
  theme(legend.position="none")

  
# Merge plots together with patchwork!
p <- pHTS1 + pHTS2 + pHTS3

ggsave("simulations/plots_RR2/HTS_Overshooting.pdf", 
       plot = p,
       width=9, height=3)

 

