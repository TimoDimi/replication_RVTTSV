library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(forcats)
library(xts)
library(highfrequency)

source("sample_schemes_est.R")
source("lambda_est.R")
source("varsigma_est.R")
source("RV_comparison.R")
source("RV_comparison_QLIKE.R")

dir_base <- getwd()

sampling_set <-  c("CTS", "TTS_realized", "BTS_rolling_avg50", "BTS_realized_rolling_avg50", "HTS")

df_RV_full_raw <- readRDS(paste0(dir_base, "/application/data/RVest_res_modified.rds"))

# Set negative RV (of RV_PAVG...) estimates to some eps_RV
eps_RV <- df_RV_full_raw %>% filter(type_estimator=="RV") %>% pull(RV) %>% min() %>% max(10^(-7)) # Use the smallest RV estimate

df_RV_full <- df_RV_full_raw %>%
  filter(sampling %in% sampling_set, type_estimator == "RV") %>%
  mutate(RV = pmax(RV, eps_RV)) %>%
  rename(M_individual=M,
         M=M_rounded) %>%
# Remove duplicates: Often two individual M values are mapped to the same M_round. Use the closest M to M_round.
  mutate(M_dist = abs(M_individual-M)) %>%
  group_by(asset, Date, sampling, type_estimator, M) %>%
  arrange(M_individual) %>%  #For ties, use the SMALLER value of M_individual as which.min picks the FIRST index (in the case of ties) that corresponds to the minimum!
  slice(which.min(M_dist)) %>%
  ungroup() %>%
  select(Date, asset, sampling, M, type_estimator, RV, M, M_individual)



# "Fill" HTS schemes?
df_dates <- df_RV_full %>% filter(sampling=="CTS") %>% select(Date, asset, M)

df_RV_full_HTSmod <- 
  bind_rows(df_RV_full %>% filter(sampling != "HTS"),
            full_join(df_RV_full %>% filter(sampling=="HTS"), df_dates)) %>%
  group_by(Date, M, asset) %>%
  mutate(incomplete_case = any(is.na(RV))) %>%
  replace_na(list(sampling = "HTS")) %>%
  ungroup()


# Fill (essentially HTS) values for values of M that are not available on some days..
df_RV_full_HTSmod <- df_RV_full_HTSmod %>%
  arrange(Date, asset, sampling, M) %>%
  group_by(Date, asset) %>%
  tidyr::fill(c(RV, type_estimator), .direction="downup") %>%
  ungroup()
  



asset_set <- df_RV_full$asset %>% unique()

M_set <- c(13, 26, 39, 78, 130, 260, 390)

type_estimator_set <- "RV"
transformation_set <- c("none")

start_time <- Sys.time()
HAR_df_full <- tibble()
for (asset_choice in asset_set){
  for (sampling_choice in sampling_set){
    for (M_choice in M_set){
      for (type_estimator_choice in type_estimator_set){
        for (transformation_choice in transformation_set){
          
          # Transform the NULL value
          transformation_choice_hlp <- transformation_choice
          if (transformation_choice == "none"){transformation_choice_hlp <- NULL}
          
          df_RV_xts <- df_RV_full_HTSmod %>%
            dplyr::filter(M==M_choice, sampling==sampling_choice, type_estimator==type_estimator_choice, asset==asset_choice) %>%
            select(Date, RV) %>%
            as.xts() 
          
          # Estimation and evaluation data set
          df_RV_xts_estimation <- df_RV_xts[index(df_RV_xts) < as.Date("2015-03-28")]
          df_RV_xts_evaluation <- df_RV_xts[index(df_RV_xts) >= as.Date("2015-03-28")]
          
          L_est <- length(df_RV_xts_estimation)
          L_eval <- length(df_RV_xts_evaluation)
          
          df_RV_xts_eval_tibble <- tibble(Date = time(df_RV_xts_evaluation),
                                          RV_obs = as.numeric(df_RV_xts_evaluation$RV),
                                          RV_FC = NA)
          
          
          # Rolling window loop: 
          for (i in 1:L_eval) {
            # Fit HAR model
            har_fit <- HARmodel(df_RV_xts[i:(L_est+i-1),], periods = c(1, 5, 22), type = "HAR", transform = transformation_choice_hlp)
            
            # Forecast the next value
            Date_FC <- df_RV_xts_eval_tibble$Date[i]
            df_RV_xts_eval_tibble$RV_FC[df_RV_xts_eval_tibble$Date == Date_FC] <- predict(har_fit, n.ahead = 1)
          }
          
          HAR_df <- df_RV_xts_eval_tibble %>%
            mutate(asset = asset_choice,
                   sampling=sampling_choice,
                   M=M_choice,
                   type_estimator=type_estimator_choice,
                   transformation=transformation_choice)
          
          HAR_df_full <- bind_rows(HAR_df_full, HAR_df)
        }
      }
    }
  }
}
end_time <- Sys.time()
(run_time <- end_time - start_time)

# Set negative RV forecasts to some small positive value!!!
HAR_df_full <- HAR_df_full %>%
  mutate(RV_FC = pmax(RV_FC, eps_RV))  
  
saveRDS(HAR_df_full, file = paste0(dir_base, "/application/data/HAR_RV_FCs_RR2.rds"))



HAR_df_full <- readRDS(file = paste0(dir_base, "/application/data/HAR_RV_FCs_RR2.rds")) %>%
  filter(sampling %in% c("CTS", "TTS_realized", "BTS_rolling_avg50", "BTS_realized_rolling_avg50", "HTS"))



###  FC evaluation against M=78 CTS RV as a common target
HAR_eval_CTS <- HAR_df_full %>%
  group_by(asset, Date) %>%
  mutate(RV_eval = RV_obs[M==78 & type_estimator=="RV" & sampling=="CTS" & transformation=="none"]) %>%  
  ungroup() %>%
  # filter(incomplete_case==FALSE) %>%
  group_by(asset, sampling, M, type_estimator, transformation) %>%
  summarize(MSE = mean((RV_eval - RV_FC)^2),
            QLIKE =  mean(RV_eval/RV_FC - log(RV_eval/RV_FC) - 1)) %>%
  ungroup()


# Summarize evaluation results by summary ranks of MSE and QLIKE losses
HAR_eval_CTS_summary <- HAR_eval_CTS %>%
  # filter(M<=390, type_estimator=="RV", transformation=="none") %>%
  filter(M<=390, type_estimator=="RV") %>%
  group_by(asset, M, type_estimator, transformation) %>%
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


# Summarize results by sampling frequency, maybe better for us!?!?!
HAR_eval_CTS_byM <- HAR_eval_CTS %>%
  filter( (M<=390 & type_estimator=="RV") ) %>%
  group_by(asset, M, type_estimator, transformation) %>%
  mutate(rank_MSE = rank(MSE),
         rank_QLIKE = rank(QLIKE)) %>%
  ungroup() %>%
  group_by(sampling, M) %>%
  summarize(mean_rk_MSE = mean(rank_MSE),
            mean_rk_QLIKE = mean(rank_QLIKE),
            perc_win_MSE = mean(rank_MSE==1),
            perc_win_QLIKE = mean(rank_QLIKE==1)) %>%
  mutate(eval_measure = "RV_CTS_5min") %>%
  arrange(M, sampling) %>%
  pivot_longer(c(mean_rk_MSE, mean_rk_QLIKE, perc_win_MSE, perc_win_QLIKE))



# Recode factor "name" (evaluation quantity)
HAR_eval_CTS_byM$name <- recode_factor(HAR_eval_CTS_byM$name,
                                       mean_rk_MSE="Mean MSE Rank", 
                                       mean_rk_QLIKE="Mean QLIKE Rank", 
                                       perc_win_MSE="Winning Percentage MSE", 
                                       perc_win_QLIKE="Winning Percentage QLIKE")

# Recode factor "sampling"
HAR_eval_CTS_byM$sampling <- recode_factor(HAR_eval_CTS_byM$sampling,
                                           CTS="CTS",
                                           TTS_realized="rTTS",
                                           BTS_rolling_avg50="iBTS",
                                           BTS_realized_rolling_avg50="rBTS", 
                                           HTS="HTS")


ggplot(HAR_eval_CTS_byM) +
  geom_line(aes(x=M, y=value, col=sampling)) +
  facet_wrap(~name, scales="free", nrow=1)  +
  scale_x_continuous(
    breaks = c(13, 78, 390),
    trans = 'log2'
  ) +
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#D73027", "#F8766D", "#C77CFF")) +
  theme(legend.position = "bottom",
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("application/plots/HAR_FC_eval_CTS.pdf", width=9, height=3.5)






###  FC evaluation against each separate target
HAR_eval_individual <- HAR_df_full %>%
  group_by(asset, sampling, M, type_estimator, transformation) %>%
  summarize(MSE = mean((RV_obs - RV_FC)^2),
            QLIKE =  mean(RV_obs/RV_FC - log(RV_obs/RV_FC) - 1)) 


# Summarize evaluation results by summary ranks of MSE and QLIKE losses
HAR_eval_individual_summary <- HAR_eval_individual %>%
  filter( (M<=390 & type_estimator=="RV") ) %>%
  group_by(asset, M, type_estimator, transformation) %>%
  mutate(rank_MSE = rank(MSE),
         rank_QLIKE = rank(QLIKE)) %>%
  ungroup()  %>%
  group_by(sampling) %>%
  summarize(mean_rk_MSE = mean(rank_MSE),
            median_rk_MSE = median(rank_MSE),
            mean_rk_QLIKE = mean(rank_QLIKE),
            median_rk_QLIKE = median(rank_QLIKE),
            mean_MSE = mean(MSE),
            median_MSE = median(MSE),
            mean_QLIKE = mean(QLIKE),
            median_QLIKE = median(QLIKE),
            perc_win_MSE = mean(rank_MSE==1),
            perc_win_QLIKE = mean(rank_QLIKE==1)) %>%
  mutate(eval_measure = "individual")


HAR_eval_individual_byM <- HAR_eval_individual %>%
  filter(M<=390, type_estimator=="RV", transformation=="none") %>%
  group_by(asset, M, type_estimator, transformation) %>%
  mutate(rank_MSE = rank(MSE),
         rank_QLIKE = rank(QLIKE)) %>%
  ungroup() %>%
  group_by(sampling, M) %>%
  summarize(mean_rk_MSE = mean(rank_MSE),
            mean_rk_QLIKE = mean(rank_QLIKE),
            perc_win_MSE = mean(rank_MSE==1),
            perc_win_QLIKE = mean(rank_QLIKE==1)) %>%
  mutate(eval_measure = "RV_CTS_5min") %>%
  arrange(M, sampling) %>%
  pivot_longer(c(mean_rk_MSE, mean_rk_QLIKE, perc_win_MSE, perc_win_QLIKE))




# Recode factor "name" (evaluation quantity)
HAR_eval_individual_byM$name <- recode_factor(HAR_eval_individual_byM$name,
                                              mean_rk_MSE="Mean MSE Rank", 
                                              mean_rk_QLIKE="Mean QLIKE Rank", 
                                              perc_win_MSE="Winning Percentage MSE", 
                                              perc_win_QLIKE="Winning Percentage QLIKE")

# Recode factor "sampling"
HAR_eval_individual_byM$sampling <- recode_factor(HAR_eval_individual_byM$sampling,
                                                  CTS="CTS",
                                                  TTS_realized="rTTS",
                                                  BTS_rolling_avg50="iBTS",
                                                  BTS_realized_rolling_avg50="rBTS", 
                                                  HTS="HTS")


ggplot(HAR_eval_individual_byM) +
  geom_line(aes(x=M, y=value, col=sampling)) +
  facet_wrap(~name, scales="free", nrow=1)  +
  scale_x_continuous(
    breaks = c(13, 78, 390),
    trans = 'log2'
  ) +
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#D73027", "#F8766D", "#C77CFF")) +
  theme(legend.position = "bottom",
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("application/plots/HAR_FC_eval_individ.pdf", width=9, height=3.5)




