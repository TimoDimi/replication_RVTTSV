# This file produces the HAR forecasts and the evaluation results for the the application including the pre-averaging RV estimator.
# Results are presented when evaluating against daily squared return values for forecast evaluation.
# For corresponding results evaluated against the Pre-AVG estimator or CTS with M=78, 
# one has to comment (out) the lines 174, 176-178 and 277, 279-281 and change the plot save command in line 420.


library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(forcats)
library(xts)
library(highfrequency)
library(MCS)

source("sample_schemes_est.R")
source("lambda_est.R")
source("varsigma_est.R")
source("RV_comparison.R")
source("RV_comparison_QLIKE.R")

dir_base <- getwd()

sampling_set <-  c("CTS", "TTS_realized", "BTS_rolling_avg50", "BTS_realized_rolling_avg50", "HTS", "PreAvg")


# Load file with RV estimates (modified in the other evaluation file)
df_RV_full_mod_RVsampling <- readRDS(file = paste0(dir_base, "/application/data/RVest_res_modified.rds")) %>%
  filter(type_estimator == "RV")

# Load file with PAVG
df_RV_PAVG <- readRDS(paste0(dir_base, "/application/data/RV_PAVG_est_starting2012.rds")) %>%
  mutate(sampling = "PreAvg",
         type_estimator="RV") %>% # Artificially set type_estimator to "RV" for the comparison below!
  crossing(M =  c(13,26,39,78,130,260,390)) %>%
  mutate(M_avg = M,
         M_avg_asset = M,
         M_avg_asset_month = M,
         M_rounded = M) %>%
  semi_join(df_RV_full_mod, by=c("Date", "asset"))

# Bind files
df_RV_full_mod <- bind_rows(df_RV_full_mod_RVsampling, df_RV_PAVG)




# Set negative RV (of RV_PAVG...) estimates to some eps_RV
eps_RV <- df_RV_full_mod %>% filter(type_estimator=="RV") %>% pull(RV) %>% min() %>% max(10^(-7)) # Use the smallest RV estimate

df_RV_full <- df_RV_full_mod %>%
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
  
saveRDS(HAR_df_full, file = paste0(dir_base, "/application/data/HAR_RV_FCs_RR3.rds"))




# Load files
HAR_df_full <- readRDS(file = paste0(dir_base, "/application/data/HAR_RV_FCs_RR3.rds")) %>%
  filter(sampling %in% c("CTS", "TTS_realized", "BTS_rolling_avg50", "BTS_realized_rolling_avg50", "HTS", "PreAvg"))


# Load daily squared returns
df_daily_squared_returns <- readRDS(paste0(dir_base, "/application/data/Daily_squared_returns.rds")) %>%
  rename(RV_obs=RV) %>%
  select(Date, RV_obs, asset, sampling, M, type_estimator) %>%
  mutate(RV_obs = pmax(RV_obs, eps_RV))



###  FC evaluation against M=78 CTS RV as a common target
HAR_eval_CTS <- HAR_df_full %>%
  bind_rows(df_daily_squared_returns) %>%
  group_by(asset, Date) %>%
  mutate(RV_eval = RV_obs[M==1 & type_estimator=="RV" & sampling=="CTS"]) %>%  
  # mutate(RV_eval = RV_obs[M==78 & type_estimator=="RV" & sampling=="PreAvg"]) %>%  
  # mutate(RV_eval = RV_obs[M==78 & type_estimator=="RV" & sampling=="CTS" & transformation=="none"]) %>%  
  ungroup() %>%
  # filter(incomplete_case==FALSE) %>%
  group_by(asset, sampling, M, type_estimator, transformation) %>%
  summarize(MSE = mean((RV_eval - RV_FC)^2),
            QLIKE =  mean(RV_eval/RV_FC - log(RV_eval/RV_FC) - 1)) %>%
  ungroup()


# Summarize evaluation results by summary ranks of MSE and QLIKE losses
HAR_eval_CTS_summary <- HAR_eval_CTS %>%
  # filter(M<=390, type_estimator=="RV", transformation=="none") %>%
  filter(M>=13 & M<=390, type_estimator=="RV") %>%
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


# Summarize results by sampling frequency
HAR_eval_CTS_byM_raw <- HAR_eval_CTS %>%
  filter( (M>=13 & M<=390 & type_estimator=="RV") ) %>%
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


# Recode factor "sampling"
HAR_eval_CTS_byM_raw$sampling <- recode_factor(HAR_eval_CTS_byM_raw$sampling,
                                           CTS="CTS",
                                           TTS_realized="rTTS",
                                           BTS_rolling_avg50="iBTS",
                                           BTS_realized_rolling_avg50="rBTS", 
                                           HTS="HTS",
                                           PreAvg="PreAvg")

HAR_eval_CTS_byM <- HAR_eval_CTS_byM_raw

# Recode factor "name" (evaluation quantity)
HAR_eval_CTS_byM$name <- recode_factor(HAR_eval_CTS_byM$name,
                                       mean_rk_MSE="Mean MSE Rank", 
                                       mean_rk_QLIKE="Mean QLIKE Rank", 
                                       perc_win_MSE="Winning Percentage MSE", 
                                       perc_win_QLIKE="Winning Percentage QLIKE")




ggplot(HAR_eval_CTS_byM) +
  geom_line(aes(x=M, y=value, col=sampling)) +
  facet_wrap(~name, scales="free", nrow=1)  +
  scale_x_continuous(
    breaks = c(13, 78, 390),
    trans = 'log2'
  ) +
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#D73027", "#F8766D", "#C77CFF","black")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

# ggsave("application/plots/HAR_FC_eval_CTS_RR3.pdf", width=9, height=3.5)












############ MCS Code

# Compute MSEs!
HAR_eval_Losses <- HAR_df_full %>%
  bind_rows(df_daily_squared_returns) %>%
  group_by(asset, Date) %>%
  mutate(RV_eval = RV_obs[M==1 & type_estimator=="RV" & sampling=="CTS"]) %>%  
  # mutate(RV_eval = RV_obs[M==78 & type_estimator=="RV" & sampling=="CTS" & transformation=="none"]) %>%  
  # mutate(RV_eval = RV_obs[M==78 & type_estimator=="RV" & sampling=="PreAvg"]) %>%  
  ungroup() %>%
  mutate(MSE = (RV_eval - RV_FC)^2,
         QLIKE =  (RV_eval/RV_FC - log(RV_eval/RV_FC) - 1))



# Start with an empty MCS tibble
MCS_results_df <- tibble(asset=NA, M=NA, loss=NA,
                         sampling_CTS=NA,  
                         sampling_TTS_realized=NA,
                         sampling_BTS_rolling_avg50=NA,
                         sampling_BTS_realized_rolling_avg50=NA,
                         sampling_HTS=NA,
                         sampling_PreAvg=NA) 

M_set_FCeval <- c(13, 26, 39, 78, 130, 260, 390)

for (asset_choice in unique(HAR_eval_Losses$asset)){
  for (M_choice in M_set_FCeval){
    for (loss_choice in c("MSE", "QLIKE")){

      # Compute MCS
      MCS_obj <- HAR_eval_Losses %>%
        select(Date, asset, M, sampling, !!loss_choice) %>%
        filter(asset==asset_choice, M==M_choice) %>%
        pivot_wider(
          names_from = sampling,
          values_from = !!loss_choice,
          names_prefix = "sampling_"
        ) %>%
        reframe(mat = list(as.matrix(across(starts_with("sampling_"))))) %>%
        pull(mat) %>% .[[1]] %>%
        MCS::MCSprocedure(alpha=0.1, B=2000)
      
    
    # Fill new rows with indicators whether sampling is in MCS or not
    row_new <- tibble(
      !!!set_names(
        as.numeric(names(MCS_results_df) %in% MCS_obj@Info$model.names),
        names(MCS_results_df)
      )
    ) %>%
      mutate(asset=asset_choice, M=M_choice, loss=loss_choice)
    
    # Bind new row to tibble
    MCS_results_df <- bind_rows(MCS_results_df, row_new)
    
    }
  }
}


# Compute MCS inclusion rates
MCS_inclusion_rates <- MCS_results_df %>% 
  na.omit() %>%
  group_by(M, loss) %>%
  summarize(across(starts_with("sampling_"), \(x) mean(x, na.rm = TRUE))) %>% 
  pivot_longer(
    cols = starts_with("sampling"),              
    names_to = "sampling",  
    values_to = "inclusion_rate"     
  )


# Recode factor "sampling"
MCS_inclusion_rates$sampling <- recode_factor(MCS_inclusion_rates$sampling,
                                              sampling_CTS="CTS",
                                              sampling_TTS_realized="rTTS",
                                              sampling_BTS_rolling_avg50="iBTS",
                                              sampling_BTS_realized_rolling_avg50="rBTS", 
                                              sampling_HTS="HTS",
                                              sampling_PreAvg="PreAvg")


ggplot(MCS_inclusion_rates) +
  geom_line(aes(x=M, y=inclusion_rate, col=sampling)) +
  facet_wrap(~loss, nrow=1)  +
  scale_x_continuous(
    breaks = c(13, 78, 390),
    trans = 'log2'
  ) +
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#D73027", "#F8766D", "#C77CFF", "black")) +
  theme(legend.position = "bottom",
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))




# A joint plot

HAR_eval_joint_df <- HAR_eval_CTS_byM_raw %>%
  separate(
  col = name,
  into = c("eval_metric", "loss"),
  sep = "_(?=[^_]+$)"   # split at the last underscore
) %>% 
  select(sampling, M, loss, eval_metric, value) %>%
  bind_rows(MCS_inclusion_rates %>% 
              mutate(eval_metric="MCS inclusion rates") %>%
              rename(value=inclusion_rate)
  )


# Recode factor "name" (evaluation quantity)
HAR_eval_joint_df$eval_metric <- recode_factor(HAR_eval_joint_df$eval_metric,
                                               "mean_rk"="Mean Rank", 
                                               "perc_win"="Winning Rates", 
                                               "MCS inclusion rates"="MCS Inclusion Rates")


# A joint plot
ggplot(HAR_eval_joint_df) +
  geom_line(aes(x=M, y=value, col=sampling)) +
  facet_wrap(loss~eval_metric, scales="free", nrow=2)  +
  scale_x_continuous(
    breaks = c(13, 78, 390),
    trans = 'log2'
  ) +
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#D73027", "#F8766D", "#C77CFF", "black")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    panel.spacing.y = unit(1, "lines")
  ) +
  guides(
    color = guide_legend(
      title = "Sampling scheme",
      nrow = 1,          # force all items into one row
      byrow = TRUE
    ),
    linetype = guide_legend(title = "Sampling scheme")
  )


ggsave("application/plots/HAR_FC_eval_RR3_vsSQRet.pdf", width=9, height=6.5)


