library(tidyverse)
library(lubridate)
library(padr)
library(slider)
library(doParallel)

source("sample_schemes_est.R")
source("simulations/sim_TTSV.R")
source("lambda_est.R")
source("varsigma_est.R")


### Simulation Setup
D <- 500 # simulation replications
D_past <- 250 # simulation days for estimating the past intensity patterns 
MC_rep_factor <- 10 # replication factor of noise settings to use more cores!
core.max <- 50


TT <- 23400
M_set <- c(2,4,6,10,13,15,18,20,26,30,36,39,45,52,60,65,78,90,117,130,156,180,195,234,260,390,468,585,780,1170,2340)


# Deterministic trends: estimated and averaged IBM data
IBM_intensities_avg <- readRDS(file = "simulations/data/IBM_intensities_est.rds")

LamSig_det <-  IBM_intensities_avg %>%
  dplyr::filter(tau!=23400) %>%
  summarize(time = 0:TT,
            lambda = approx(x=tau, y=lambda_avg, xout=0:TT, rule=2)$y,
            varsigma = approx(x=tau, y=varsigma_avg, xout=0:TT, rule=2)$y)



################################################################################
################################################################################
###     PLOTTING

### Plot true lambda and varsigma curves for a simulated test set
set.seed(1)
dat_TTSV_test <- sim_TTSV(days=5,
                          lambda_det=LamSig_det$lambda, 
                          varsigma_det=LamSig_det$varsigma, 
                          TT=TT+1)

# Plotting df for the simulated days
df_plot_sim <- dat_TTSV_test$prices %>% 
  filter(Date<=3) %>%
  mutate(varsigma2 = varsigma^2,
         sigma2 = lambda*varsigma2) %>%
  select(Date, SecSinceStart, Price, lambda, varsigma2, sigma2) %>%
  pivot_longer(cols=c("Price", "lambda", "varsigma2", "sigma2"), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(Day=Date,
         Time=hms::as_hms(SecSinceStart + 34200),
         measure = factor(measure, levels=c("Price", "sigma2", "lambda", "varsigma2"))) %>%
  select(Time, Day, measure, value)


# Plotting df for the deterministic intensities
df_plot_intensities <- LamSig_det %>% 
  mutate(varsigma2 = varsigma^2,
         sigma2 = lambda*varsigma2) %>%
  select(-varsigma) %>%
  pivot_longer(cols=c("lambda", "varsigma2", "sigma2"), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(Time = hms::as_hms(time + 34200),
         measure = factor(measure, levels=c("sigma2", "lambda", "varsigma2"))) %>%
  mutate(Day=10000) %>%
  select(Time, Day, measure, value)

# Join df's
df_plot <- bind_rows(df_plot_sim, df_plot_intensities) %>%
  mutate(Day=factor(Day))

# Set factor levels to obtain nice facet captions
levels(df_plot$measure) <- c("Simulated~price",
                             expression(Simulated~spot~variance~sigma^2*(t)),
                             expression(Simulated~tick~intensity~lambda(t)),
                             expression(Simulated~tick~variance~varsigma^2*(t)))

# Plot the first few days of price/lambda/varsigma days
ggplot(df_plot) +
  geom_line(aes(x=Time, y=value, col=Day)) +
  facet_wrap(~measure, ncol=2, scales="free", labeller=label_parsed) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("#D39200","#00C19F", "#DB72FB", "black")) +
  scale_x_time(breaks = scales::date_breaks("1 hours"),
               labels = scales::time_format(format="%H:%M"))


ggsave("simulations/plots/Sim_Price_Path.pdf", width=8, height=6)

################################################################################
################################################################################

# Noise setting
dat_TTSV_test$intensities %>% summarize(mean_vs = mean(varsigma))

noise_settings_list <- list(
  c(sd_noise=0, MAcoeff_noise=0),
  c(sd_noise=0.3*10^(-4), MAcoeff_noise=0),
  c(sd_noise=1.2*10^(-4), MAcoeff_noise=0),
  c(sd_noise=0.3*10^(-4)/(1+0.6^2), MAcoeff_noise=-0.6),
  c(sd_noise=1.2*10^(-4)/(1+0.6^2), MAcoeff_noise=-0.6))

# Run on more kernels with this replication trick!!!
noise_settings_list <- rep(noise_settings_list, MC_rep_factor)
noise_setting_names <- rep(c("no", "low_iid", "high_iid", "low_MA1", "high_MA1"), MC_rep_factor)


# Cluster Settings
cl <- makeCluster(min(parallel::detectCores()-1, core.max, length(noise_settings_list)) )
registerDoParallel(cl)

time_start <- Sys.time()
# Loop over different settings for the MMN
res_df <- foreach(
  i_noise_setting = 1:length(noise_settings_list),
  .combine=rbind,
  .packages=c("tidyverse", "lubridate", "padr", "slider"),
  .errorhandling="pass"
)%dopar%{
  
  source("sample_schemes_est.R")
  source("simulations/sim_TTSV.R")
  source("lambda_est.R")
  source("varsigma_est.R")
  
  set.seed(i_noise_setting)
  noise_setting <- noise_settings_list[[i_noise_setting]]
  
  ### Simulate TTSV model around the average estimated IBM intensities
  dat_TTSV_sim <- sim_TTSV(days=D,
                           lambda_det=LamSig_det$lambda, 
                           varsigma_det=LamSig_det$varsigma, 
                           TT=TT+1)
  
  # Add noise
  sd_noise <- noise_setting[1]
  MAcoeff_noise <- noise_setting[2]
  
  
  dat_TTSV_sim$prices <- dat_TTSV_sim$prices %>% 
    group_by(Date) %>%
    mutate(nu_noise = rnorm(length(Price), mean=0, sd=sd_noise),
           eps_noise = nu_noise + MAcoeff_noise*(replace(lag(nu_noise), is.na(lag(nu_noise)), 0)),
           LogPrice = LogPrice + eps_noise,
           Price = exp(LogPrice))
  
  # Calculate the true IV
  df_IV <- dat_TTSV_sim$intensities %>%
    group_by(Date) %>%
    summarise(IV = sum(lambda*varsigma^2))
  
  # Simulate pre-sample observations to estimate past TTS and BTS
  dat_TTSV_PreSample <- sim_TTSV(days=D_past,
                                 lambda_det=LamSig_det$lambda, 
                                 varsigma_det=LamSig_det$varsigma, 
                                 TT=TT+1)
  
  # Apply the sampling schemes on the simulated returns
  df_sim <- bind_rows(CTS(dat_TTSV_sim$prices) %>% mutate(sampling="CTS"),
                      TTS_true(dat_TTSV_sim$prices) %>% mutate(sampling="TTS_true"),
                      BTS_true(dat_TTSV_sim$prices) %>% mutate(sampling="BTS_true"),
                      TTS_daily(dat_TTSV_sim$prices) %>% mutate(sampling="TTS_daily"),
                      BTS_daily(dat_TTSV_sim$prices) %>% mutate(sampling="BTS_daily"),
                      TTS_past(df_prices=dat_TTSV_sim$prices, 
                               df_est = dat_TTSV_PreSample$prices) %>% mutate(sampling="TTS_past"),
                      BTS_past(df_prices=dat_TTSV_sim$prices, 
                               df_est = dat_TTSV_PreSample$prices) %>% mutate(sampling="BTS_past"),
                      TTS_rolling_days(df_prices=dat_TTSV_sim$prices, days_RollMean=1) %>% mutate(sampling="TTS_rolling1"),
                      BTS_rolling_days(df_prices=dat_TTSV_sim$prices, days_RollMean=1) %>% mutate(sampling="BTS_rolling1")) %>%
    group_by(Date, sampling) %>%
    mutate(return = log(Price) - lag(log(Price)))
  
  # Aggregate returns to higher frequencies and compute returns
  df_RV <- tibble()
  for (M in M_set){
    n_aggregate <- TT/(10*M)
    
    df_tmp <- df_sim %>%
      drop_na(return) %>% # Only remove NAs in the return column!s
      group_by(Date, sampling) %>%
      summarize(Date=Date,
                sampling=sampling,
                days_past=days_past,
                time_sampling=time_sampling + seconds(10*(n_aggregate-1)),
                return = slider::slide_sum(x=return,
                                           before=0,
                                           after=n_aggregate-1,
                                           step=n_aggregate)) %>%
      drop_na(return)
    
    df_tmp2 <- bind_rows(
      df_tmp %>%
        group_by(Date, sampling) %>%
        summarize(RV = sum(return^2, na.rm=TRUE),
                  M = M,
                  type_estimator = "RV"),
      df_tmp %>%
        group_by(Date, sampling) %>%
        summarize(RV = sum(return^2, na.rm=TRUE) + 
                    M/(M-1)*sum(return*lag(return), na.rm=TRUE) + 
                    M/(M-1)*sum(return*lead(return), na.rm=TRUE),
                  M = M,
                  type_estimator = "RV_AC1"))
    
    df_RV <- bind_rows(df_RV, df_tmp2)
  }
  
  df_est_tmp <- left_join(df_RV, df_IV, by=c("Date")) %>%
    mutate(sd_noise=sd_noise,
           MAcoeff_noise=MAcoeff_noise,
           i_noise_setting=i_noise_setting,
           noise_setting_name=noise_setting_names[i_noise_setting])
  
  df_est_tmp
}
stopCluster(cl)

time_end <- Sys.time()
(run_time <- time_end - time_start)


# Safe estimated file
saveRDS(res_df, file = "simulations/data/sim_20221115.rds")

 
