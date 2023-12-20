library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(forcats)
library(xts)
library(highfrequency)
library(HARModel)

source("sample_schemes_est.R")
source("lambda_est.R")
source("varsigma_est.R")
source("RV_comparison.R")
source("RV_comparison_QLIKE.R")

dir_base <- getwd()

# Read RV estimates from the individual files:
# file list RVs:
data_files_RVest <- list.files(path="application/data/RV_est/", full.names=TRUE, recursive=FALSE)

df_RV_full <- tibble()
for (file in data_files_RVest){
  df_tmp <- readRDS(file)
  df_RV_full <- bind_rows(df_RV_full, df_tmp)
}

df_RV_full <- df_RV_full %>% 
  dplyr::filter(Date >= as_date("2012-01-01"), 
                Date <= as_date("2019-12-31")) %>%
  dplyr::filter(Date !="2015-08-24") %>% # Bad day for many stocks
  dplyr::filter( !(Date =="2015-11-10" & asset == "MCD")) # Bad day for MCD


# Set negative RV (of RV_PAVG...) estimates to some eps_RV
eps_RV <- df_RV_full %>% filter(type_estimator=="RV") %>% pull(RV) %>% min() %>% max(10^(-7)) # Use the smallest RV estimate
df_RV_full <- df_RV_full %>% 
  mutate(RV = pmax(RV, eps_RV)) 


asset_set <- df_RV_full$asset %>% unique()
sampling_set <- df_RV_full$sampling %>% unique()
M_set <- c(13, 26, 39, 78, 260, 390, 780, 2340, 4680)
type_estimator_set <- df_RV_full$type_estimator %>% unique()

HAR_df_full <- tibble()
for (asset_choice in asset_set){
  for (sampling_choice in sampling_set){
    for (M_choice in M_set){
      for (type_estimator_choice in type_estimator_set){
        df_RV_xts <- df_RV_full %>%
          dplyr::filter(M==M_choice, sampling==sampling_choice, type_estimator==type_estimator_choice, asset==asset_choice) %>%
          select(Date, RV) %>%
          as.xts() 
        
        HAR_FC <- HARModel::HARForecast(df_RV_xts, nRoll=1000, nAhead=1)
        
        HAR_df <- tibble(Date = HAR_FC@data$forecastDates,
                         RV_obs = as.numeric(HAR_FC@data$forecastComparison$RV),
                         RV_FC = as.numeric(HAR_FC@forecast)) %>%
          mutate(asset = asset_choice,
                 sampling=sampling_choice,
                 M=M_choice,
                 type_estimator=type_estimator_choice)
                         
        HAR_df_full <- bind_rows(HAR_df_full, HAR_df)
      }
    }
  }
}

# Set negative RV forecasts to some small positive value!!!
HAR_df_full <- HAR_df_full %>%
  mutate(RV_FC = pmax(RV_FC, eps_RV))  
  

saveRDS(HAR_df_full, file = paste0(dir_base, "/application/data/HAR_RV_FCs.rds"))





HAR_df_full <- readRDS(file = paste0(dir_base, "/application/data/HAR_RV_FCs.rds"))


###  FC evaluation against M=78 CTS RV as a common target
HAR_eval_CTS <- HAR_df_full %>%
  group_by(asset, Date) %>%
  mutate(RV_eval = RV_obs[M==78 & type_estimator=="RV" & sampling=="CTS"]) %>%  
  ungroup %>%
  group_by(asset, sampling, M, type_estimator) %>%
  summarize(MSE = mean((RV_eval - RV_FC)^2),
            QLIKE =  mean(RV_eval/RV_FC - log(RV_eval/RV_FC) - 1)) 


# Summarize evaluation results by summary ranks of MSE and QLIKE losses
HAR_eval_CTS_summary <- HAR_eval_CTS %>%
  filter( (M<=390 & type_estimator=="RV") | (M>=78 & type_estimator=="RV_PAVG")) %>%
  group_by(asset, M, type_estimator) %>%
  mutate(rank_MSE = rank(MSE),
         rank_QLIKE = rank(QLIKE)) %>%
  ungroup() %>%
  group_by(sampling) %>%
  summarize(mean_rk_MSE = mean(rank_MSE),
            median_rk_MSE = median(rank_MSE),
            mean_rk_QLIKE = mean(rank_QLIKE),
            median_rk_QLIKE = median(rank_QLIKE),
            mean_MSE = mean(MSE),
            mean_QLIKE = mean(QLIKE),
            perc_win_MSE = mean(rank_MSE==1),
            perc_win_QLIKE = mean(rank_QLIKE==1)) %>%
  mutate(eval_measure = "RV_CTS_5min")





###  FC evaluation against each separate target
HAR_eval_individual <- HAR_df_full %>%
  group_by(asset, sampling, M, type_estimator) %>%
  summarize(MSE = mean((RV_obs - RV_FC)^2),
            QLIKE =  mean(RV_obs/RV_FC - log(RV_obs/RV_FC) - 1)) 


# Summarize evaluation results by summary ranks of MSE and QLIKE losses
HAR_eval_individual_summary <- HAR_eval_individual %>%
  filter( (M<=390 & type_estimator=="RV") | (M>=78 & type_estimator=="RV_PAVG")) %>%
  group_by(asset, M, type_estimator) %>%
  mutate(rank_MSE = rank(MSE),
         rank_QLIKE = rank(QLIKE)) %>%
  ungroup()  %>%
  group_by(sampling) %>%
  summarize(mean_rk_MSE = mean(rank_MSE),
            median_rk_MSE = median(rank_MSE),
            mean_rk_QLIKE = mean(rank_QLIKE),
            median_rk_QLIKE = median(rank_QLIKE),
            mean_MSE = mean(MSE),
            mean_QLIKE = mean(QLIKE),
            perc_win_MSE = mean(rank_MSE==1),
            perc_win_QLIKE = mean(rank_QLIKE==1)) %>%
  mutate(eval_measure = "individual")





###  FC evaluation against squared return!
df_r2 <- df_RV_full %>% 
  filter(sampling=="CTS",
         M==1,
         type_estimator=="RV",
         Date >= min(HAR_df_full$Date)) %>%
  select(Date, asset, RV) %>%
  rename(rsq = RV) %>%
  arrange(Date, asset)


HAR_eval_rsq <- HAR_df_full %>%
  arrange(Date, asset) %>%
  left_join(df_r2, by=c("Date", "asset")) %>%
  group_by(asset, sampling, M, type_estimator) %>%
  summarize(MSE = mean((rsq - RV_FC)^2),
            QLIKE =  mean(rsq/RV_FC - log(rsq/RV_FC) - 1)) 


# Summarize evaluation results by summary ranks of MSE and QLIKE losses
HAR_eval_rsq_summary <- HAR_eval_rsq %>%
  filter( (M<=390 & type_estimator=="RV") | (M>=78 & type_estimator=="RV_PAVG")) %>%
  group_by(asset, M, type_estimator) %>%
  mutate(rank_MSE = rank(MSE),
         rank_QLIKE = rank(QLIKE)) %>%
  ungroup() %>%
  group_by(sampling) %>%
  summarize(mean_rk_MSE = mean(rank_MSE),
            median_rk_MSE = median(rank_MSE),
            mean_rk_QLIKE = mean(rank_QLIKE),
            median_rk_QLIKE = median(rank_QLIKE),
            mean_MSE = mean(MSE),
            mean_QLIKE = mean(QLIKE),
            perc_win_MSE = mean(rank_MSE==1),
            perc_win_QLIKE = mean(rank_QLIKE==1)) %>%
  mutate(eval_measure = "rsq")




HAR_eval <- bind_rows(HAR_eval_CTS_summary,
                      HAR_eval_rsq_summary,
                      HAR_eval_individual_summary) 


HAR_eval$sampling <- factor(HAR_eval$sampling, 
                            levels = c("BTS_realized_rolling_avg50", "BTS_rolling_avg50", "TTS_realized", "TTS_rolling_avg50", "CTS"))    
levels(HAR_eval$sampling) <- c("rBTS", "iBTS", "rTTS", "iTTS", "CTS")


HAR_eval %>%
  mutate(empty1=NA, empty2=NA, 
         mean_MSE= mean_MSE*10^7) %>%
  select(eval_measure, sampling, empty1, mean_rk_MSE, mean_MSE, perc_win_MSE, empty2, mean_rk_QLIKE, mean_QLIKE, perc_win_QLIKE) %>%
  xtable::xtable(digits=c(0,0,0,0,2,2,2,0,2,2,2)) %>%
  print(file="application/plots/HAR_forecasting_results.txt", include.rownames=FALSE, booktabs=TRUE)


