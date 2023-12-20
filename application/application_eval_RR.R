library(multidplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(forcats)
library(xtable)

source("sample_schemes_est.R")
source("simulations/sim_TTSV.R")
source("lambda_est.R")
source("varsigma_est.R")
source("RV_comparison.R")
source("RV_comparison_QLIKE.R")


dir_base <- getwd()

# Read the individual files with RV estimates
# file list RVs:
data_files_RVest <- list.files(path="application/data/RV_est/", full.names=TRUE, recursive=FALSE)

df_RV_full <- tibble()
for (file in data_files_RVest){
  df_tmp <- readRDS(file)
  df_RV_full <- bind_rows(df_RV_full, df_tmp)
}


# Just plot a choice of common M values
M_set_full <- c(13,26,39,78,260,390,780,2340,4680)


estim_set <- c("RV", "RV_PAVG")
baseline_sampling_set <- c("CTS", "TTS_realized")

df_CompPosNeg <- tibble()

for (choice_est in estim_set){
  
  if (choice_est == "RV"){
    M_proxy_choice <- 78
    M_set <-  c(13,26,39,78,260,390)
  } else {
    M_proxy_choice <- 4680
    M_set <- c(78,260,390,780,2340,4680)
  }
  
  for (choice_baseline_sampling in baseline_sampling_set){
    
    df_RV <- df_RV_full %>% 
      dplyr::filter(Date >= as_date("2012-01-01"), 
                    Date <= as_date("2019-12-31")) %>%
      dplyr::filter(Date !="2015-08-24") %>% # Bad day for many stocks
      dplyr::filter( !(Date =="2015-11-10" & asset == "MCD")) # Bad day for MCD

    # Fix a baseline and a proxy. The proxy is however more important than the baseline.
    # The first mutate replicates one RV value per group for the baseline (and RV_proxy_hlp)
    # The second mutate call "leads" the RV_proxy_hlp by Date
    df_RV_proxy <- df_RV %>%
      group_by(asset, Date) %>%
      mutate(RV_proxy_hlp = RV[M==78 & type_estimator=="RV" & sampling=="CTS"]) %>%  
      ungroup() %>%
      group_by(asset, Date, M) %>%
      mutate(RV_baseline = RV[type_estimator==choice_est & sampling==choice_baseline_sampling]) %>%
      dplyr::filter(M %in% M_set,
                    type_estimator==choice_est) %>%
      ungroup() %>%
      group_by(asset, sampling, days_avg, M, type_estimator) %>%
      mutate(RV_proxy = lead(RV_proxy_hlp,
                             order_by=Date))
    
    
    # Create a cluster and add the essential packages and functions/variables to each worker
    cluster <- new_cluster(9)
    cluster %>%
      cluster_library("tidyverse") %>%
      cluster_library("tibble") %>%
      cluster_library("sandwich") %>%
      cluster_copy("RV_comparison") %>%
      cluster_copy("RV_comparison_QLIKE") 
    
    
    # Actual evaluation in parallel
    set.seed(1)
    
    # (R)MSE evaluation
    df_RV_eval <- df_RV_proxy %>%
      group_by(asset, sampling, days_avg, M, type_estimator) %>%
      partition(cluster) %>% 
      summarize(RV_comparison(RV = RV,
                              RV_baseline = RV_baseline,
                              IV_proxy = RV_proxy,
                              B=1000),
                n=n()) %>%
      collect()
    
    # QLIKE evaluation
    df_RV_eval_QLIKE <- df_RV_proxy %>%
      group_by(asset, sampling, days_avg, M, type_estimator) %>%
      partition(cluster) %>% 
      summarize(RV_comparison_QLIKE(RV = RV,
                                    RV_baseline = RV_baseline,
                                    IV_proxy = RV_proxy,
                                    B=1000),
                n=n()) %>%
      collect()
    
    # Close cluster
    rm(cluster)
    
      
    # Transform into plotting data frames
    sig_level <- 0.05
    
    # MSE
    df_plot_RV_eval <- df_RV_eval %>%
      dplyr::filter(type_estimator==choice_est) %>%
      mutate(sign = ifelse(mean_loss_diff > 0, "positive", "negative"),
             RMSE_rel=pmax(pmin(RMSE_rel,0.2),-0.2),
             significant_logical = as.logical(p_val <= sig_level),
             asset = factor(asset),
             M = factor(M)) %>%
      dplyr::filter(M %in% M_set,
                    days_avg %in% c(NA, 50),
                    !sampling %in% choice_baseline_sampling)
    
    # QLIKE
    df_plot_RV_eval_QLIKE <- df_RV_eval_QLIKE %>%
      dplyr::filter(type_estimator==choice_est) %>%
      mutate(sign = ifelse(mean_loss_diff > 0, "positive", "negative"),
             QLIKE_rel=pmax(pmin(QLIKE_rel,0.5),-0.5),
             significant_logical = as.logical(p_val <= sig_level),
             asset = factor(asset),
             M = factor(M)) %>%
      dplyr::filter(M %in% M_set,
                    days_avg %in% c(NA, 50),
                    !sampling %in% choice_baseline_sampling) 
    
    
    # Collect amount of positive and negative significant results
    df_CompPosNeg <- bind_rows(
      df_CompPosNeg,
      df_plot_RV_eval %>% 
        group_by(sampling, type_estimator) %>%      
        summarize(significant_pos = mean(p_val <= sig_level & RMSE_rel>0),
                  significant_neg = mean(p_val <= sig_level & RMSE_rel<0)) %>%
        mutate(loss = "RMSE",
               baseline_sampling = choice_baseline_sampling),
      df_RV_eval_QLIKE %>% 
        group_by(sampling, type_estimator) %>%      
        summarize(significant_pos = mean(p_val <= sig_level & QLIKE_rel>0),
                  significant_neg = mean(p_val <= sig_level & QLIKE_rel<0)) %>%
        mutate(loss = "QLIKE",
               baseline_sampling = choice_baseline_sampling)
    )
    
    
    
    # Adapt factor labels for plotting
    if (choice_baseline_sampling == "CTS"){
      df_plot_RV_eval_QLIKE$sampling <- df_plot_RV_eval$sampling <- factor(df_plot_RV_eval$sampling, 
                                                                           levels = c("BTS_realized_rolling_avg50", 
                                                                                      "BTS_rolling_avg50", 
                                                                                      "TTS_realized",
                                                                                      "TTS_rolling_avg50"))
      
      levels(df_plot_RV_eval_QLIKE$sampling) <- levels(df_plot_RV_eval$sampling) <- c('Realized~BTS',
                                                                                      'Intensity~BTS',
                                                                                      'Realized~TTS',
                                                                                      'Intensity~TTS')
      
    } else {
      df_plot_RV_eval_QLIKE$sampling <- df_plot_RV_eval$sampling <- factor(df_plot_RV_eval$sampling, 
                                                                           levels = c("BTS_realized_rolling_avg50", 
                                                                                      "BTS_rolling_avg50", 
                                                                                      "TTS_rolling_avg50", 
                                                                                      "CTS"))
      
      levels(df_plot_RV_eval_QLIKE$sampling) <- levels(df_plot_RV_eval$sampling) <- c('Realized~BTS',
                                                                                      'Intensity~BTS',
                                                                                      'Intensity~TTS', 
                                                                                      'CTS')
    }
    
    
    ## Plot points with magnitude of RMSE gains/losses as a color scale!
    ggplot(df_plot_RV_eval %>% filter(significant_logical==T)) + 
      geom_point(aes(x=M,
                     y=asset,
                     col=RMSE_rel), 
                 size=3) +
      facet_wrap(~sampling, ncol=4, labeller=label_parsed) +
      scale_color_gradient2(low = "red", mid = "white", high = "black", 
                            space = "Lab",
                            labels = scales::percent) +
      scale_y_discrete(limits = rev) +
      theme_bw() +
      ylab("Asset") +
      theme(legend.position = "bottom") +
      guides(col = guide_colourbar(barwidth = 15, barheight = 1, title="Relative RMSE Improvement    "))
    
    ggsave(paste0("application/plots/appl_eval_MSE_",choice_est,"_",choice_baseline_sampling,".pdf"), width=8, height=6)
    
    
    
    ## Plot points with magnitude of RMSE gains/losses as a color scale!
    ggplot(df_plot_RV_eval_QLIKE %>% filter(significant_logical==T)) + 
      geom_point(aes(x=M,
                     y=asset,
                     col=QLIKE_rel), 
                 size=3) +
      facet_wrap(~sampling, ncol=4, labeller=label_parsed) +
      scale_color_gradient2(low = "red", mid = "white", high = "black", 
                            space = "Lab",
                            labels = scales::percent) +
      scale_y_discrete(limits = rev) +
      theme_bw() +
      ylab("Asset") +
      theme(legend.position = "bottom") +
      guides(col = guide_colourbar(barwidth = 15, barheight = 1, title="Relative QLIKE Improvement    "))
    
    ggsave(paste0("application/plots/appl_eval_QLIKE_",choice_est,"_",choice_baseline_sampling,".pdf"), width=8, height=6)
    

  }
}



# Sampling as factor
df_CompPosNeg$sampling <- factor(df_CompPosNeg$sampling, 
                                 levels = c("BTS_realized_rolling_avg50", "BTS_rolling_avg50", "TTS_realized", "TTS_rolling_avg50", "CTS"))    
levels(df_CompPosNeg$sampling) <- c("rBTS", "iBTS", "rTTS", "iTTS", "CTS")


# Print a table with amount of positive/negative losses
df_CompPosNeg_pretty <- df_CompPosNeg %>%
  rename(pos=significant_pos, neg=significant_neg) %>%
  mutate(pos = 100*pos,
         neg = 100*neg) %>%
  mutate(col_id = paste0(type_estimator,"_",loss)) %>%
  select(baseline_sampling, sampling, col_id, pos, neg) %>%
  pivot_wider(names_from = col_id,
              values_from = c(pos,neg),
              names_vary="slowest") %>%
  arrange(baseline_sampling, sampling) 


df_CompPosNeg_pretty %>%
  mutate(empty1=NA, empty2=NA, empty3=NA, empty4=NA) %>%
  dplyr::select(baseline_sampling, sampling, empty1,
                pos_RV_RMSE, neg_RV_RMSE, empty2,
                pos_RV_QLIKE, neg_RV_QLIKE, empty3,
                pos_RV_PAVG_RMSE, neg_RV_PAVG_RMSE, empty4,
                pos_RV_PAVG_QLIKE, neg_RV_PAVG_QLIKE) %>%
  xtable::xtable(digits=c(0,0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0)) %>%
  print(file="application/plots/PosNegValues.txt", include.rownames=FALSE, booktabs=TRUE)

