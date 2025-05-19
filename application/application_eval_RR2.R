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
data_files_RVest <- list.files(path="application/data/RV_est_RR2/", full.names=TRUE, recursive=FALSE)

df_RV_full_raw <- tibble()
for (file in data_files_RVest){
  df_tmp <- readRDS(file)
  df_RV_full_raw <- bind_rows(df_RV_full_raw, df_tmp)
}


## Just plot a choice of common M values
M_set_full <- c(13,26,39,78,130,260,390,780,1170,1560,2340,4680)


#####################################################################################
#####################################################################################
#####################################################################################
###   Round M values to neirest M_set for HTS and stopping time sampling schemes


# Rounding helper function (vectorized)
round_own <- function(numbers, rounding_set){
  low <- findInterval(numbers, rounding_set)
  high <- low+1 
  low.diff <- numbers - rounding_set[ifelse(low==0,NA,low)]
  high.diff <- rounding_set[ifelse(high==0,NA,high)] - numbers
  mins <- pmin(low.diff,high.diff,na.rm=T) 
  pick <- ifelse(!is.na(low.diff) & mins==low.diff, low, high)
  return(rounding_set[pick])
  # return(list(rounded_numbers=rounding_set[pick], rounding_number_index=pick))
}



df_RV_full_raw <- df_RV_full_raw %>% 
  filter(M >= 7) %>% 
  mutate(Month = month(Date), Year = year(Date))


# Modify "stopping" and "HTS" sampling schemes with different aggregate information of M
df_RV_full_mod <- bind_rows(
  df_RV_full_raw %>%
    filter(!str_detect(sampling, "stopping") & sampling!="HTS") %>%
    mutate(M_avg = M,
           M_avg_asset = M,
           M_avg_asset_month=M),
  df_RV_full_raw %>%
    filter(sampling == "HTS") %>%
    group_by(sampling, type_estimator, delta) %>%
    mutate(M_avg = mean(M)) %>%
    ungroup() %>% group_by(sampling, type_estimator, asset, delta) %>%
    mutate(M_avg_asset = mean(M)) %>%
    ungroup() %>% group_by(sampling, type_estimator, asset, delta, Month, Year) %>%
    mutate(M_avg_asset_month = mean(M)),
  df_RV_full_raw %>%
    filter(str_detect(sampling, "stopping")) %>%
    group_by(sampling, type_estimator, n_aggregate) %>%     # I.e., average M over all days, all assets, 
    mutate(M_avg = mean(M)) %>%
    ungroup() %>% group_by(sampling, type_estimator, asset, n_aggregate) %>%   # I.e., average M over all days,
    mutate(M_avg_asset = mean(M)) %>%
    ungroup() %>% group_by(sampling, type_estimator, asset, n_aggregate, Month, Year) %>%    # I.e., average M over (all days in a month), 
    mutate(M_avg_asset_month = mean(M))
)


# Rounde the M's to the M_set provided!
df_RV_full_mod <- df_RV_full_mod %>%
  # filter(str_detect(sampling, "stopping") | sampling=="HTS") %>%
  mutate(M_rounded = round_own(M, rounding_set=M_set_full),
         M_avg_rounded = round_own(M_avg, rounding_set=M_set_full),
         M_avg_asset_rounded = round_own(M_avg_asset, rounding_set=M_set_full),
         M_avg_asset_month_rounded = round_own(M_avg_asset_month, rounding_set=M_set_full))



saveRDS(df_RV_full_mod, file = paste0(dir_base, "/application/data/RVest_res_modified.rds"))





#####################################################################################
#####################################################################################
#####################################################################################




df_RV_full_mod <- readRDS(file = paste0(dir_base, "/application/data/RVest_res_modified.rds"))


estim_set <- c("RV")
baseline_sampling_set <- c("CTS", "BTS_realized_rolling_avg50")
aggregation_methods <- c("none", "monthly", "alltime", "asset_alltime")
sampling_schemes_compare <- c("CTS", "TTS_realized", "BTS_rolling_avg50", "BTS_realized_rolling_avg50", "HTS")


df_CompPosNeg <- tibble()

# Loop over aggregation methods 
for (aggregation_method in aggregation_methods){
  
  # Aggregation variant for HTS and stopping time based schemes
  if (aggregation_method == "none"){
    df_RV_full_duplicates <- df_RV_full_mod %>%
      rename(M_individual = M, M = M_rounded)   # Setting: Round every (delta, asset, date) INDIVIDUALLY to its closest value in the given M_set
  } else if (aggregation_method == "monthly"){
    df_RV_full_duplicates <- df_RV_full_mod %>%
      select(-M) %>%
      rename(M_individual = M_avg_asset_month, M = M_avg_asset_month_rounded) # Setting: Round every (delta, asset, month) but AVERAGED OVER ALL DAYS IN THAT MONTH to its closest value in the given M_set
  } else if (aggregation_method == "alltime"){
    df_RV_full_duplicates <- df_RV_full_mod %>%
      select(-M) %>%
      rename(M_individual = M_avg_asset, M = M_avg_asset_rounded)   # Setting: Round every (delta, asset) but AVERAGED OVER ALL DAYS to its closest value in the given M_set
  } else if (aggregation_method == "asset_alltime"){
    df_RV_full_duplicates <- df_RV_full_mod %>%
      select(-M) %>%
      rename(M_individual = M_avg, M = M_avg_rounded)   # Setting: Round every (delta) but AVERAGED OVER ALL DAYS AND ASSETS to its closest value in the given M_set
  }

  
  # Remove duplicates: Often two individual M values are mapped to the same M_round. Use the closest M to M_round.
  df_RV_full <- df_RV_full_duplicates %>%
    mutate(M_dist = abs(M_individual-M)) %>%
    group_by(asset, Date, sampling, type_estimator, M) %>%
    arrange(M_individual) %>%  #For ties, use the SMALLER value of M_individual as which.min picks the FIRST index (in the case of ties) that corresponds to the minimum!
    slice(which.min(M_dist)) %>%
    ungroup()
  
  
  # Filter bad days
  df_RV <- df_RV_full %>% 
    dplyr::filter(Date >= "2012-01-01",
                  Date <= "2019-12-31") %>%
    dplyr::filter(Date != "2015-08-24") %>% # Bad day for many stocks
    dplyr::filter( !(Date == "2015-11-10" & asset == "MCD")) # Bad day for MCD
  

  # Loop over different estimators (RV and PVAG)
  for (choice_est in estim_set){
    
    if (choice_est == "RV"){
      M_proxy_choice <- 78
      M_set <-  c(13,26,39,78,130,260,390)
    } else {
      M_proxy_choice <- 4680
      M_set <- c(78,260,390,780,2340,4680)
    }
    
    
    # Loop over different baseline sampling schemes
    for (choice_baseline_sampling in baseline_sampling_set){
    
      # Fix a baseline and a proxy. The proxy is however more important than the baseline.
      # The first mutate replicates one RV value per group for the baseline (and RV_proxy_hlp)
      # The second mutate call "leads" the RV_proxy_hlp by Date
      df_RV_proxy <- df_RV %>%
        filter(sampling %in% sampling_schemes_compare) %>%
        group_by(asset, Date) %>%
        mutate(RV_proxy_hlp = RV[M==M_proxy_choice & type_estimator=="RV" & sampling=="CTS"]) %>%  
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
      cluster <- new_cluster(14)
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
                                B=5000),
                  n=n()) %>%
        collect() %>%
        dplyr::filter(!(sampling %in% choice_baseline_sampling))
      
      
      # QLIKE evaluation
      df_RV_eval_QLIKE <- df_RV_proxy %>%
        group_by(asset, sampling, days_avg, M, type_estimator) %>%
        partition(cluster) %>% 
        summarize(RV_comparison_QLIKE(RV = RV,
                                      RV_baseline = RV_baseline,
                                      IV_proxy = RV_proxy,
                                      B=5000),
                  n=n()) %>%
        collect() %>%
      dplyr::filter(!(sampling %in% choice_baseline_sampling))
      
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
                      !(sampling %in% choice_baseline_sampling))
      
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
                      !(sampling %in% choice_baseline_sampling))
      
      
      # Collect amount of positive and negative significant results
      df_CompPosNeg <- bind_rows(
        df_CompPosNeg,
        df_plot_RV_eval %>% 
          group_by(sampling, type_estimator) %>%      
          summarize(significant_pos = mean(p_val <= sig_level & RMSE_rel>0),
                    significant_neg = mean(p_val <= sig_level & RMSE_rel<0)) %>%
          mutate(loss = "RMSE",
                 baseline_sampling = choice_baseline_sampling,
                 aggreg_method=aggregation_method),
        df_RV_eval_QLIKE %>% 
          group_by(sampling, type_estimator) %>%      
          summarize(significant_pos = mean(p_val <= sig_level & QLIKE_rel>0),
                    significant_neg = mean(p_val <= sig_level & QLIKE_rel<0)) %>%
          mutate(loss = "QLIKE",
                 baseline_sampling = choice_baseline_sampling,
                 aggreg_method=aggregation_method)
      )
      
      
      
      # Adapt factor labels for plotting
      if (choice_baseline_sampling == "CTS"){
        df_plot_RV_eval_QLIKE$sampling <- df_plot_RV_eval$sampling <- factor(df_plot_RV_eval$sampling, 
                                                                             levels = c("TTS_rolling_avg50",
                                                                                        "TTS_realized",
                                                                                        "TTS_realized_stopping",
                                                                                        "BTS_rolling_avg50", 
                                                                                        "BTS_realized_rolling_avg50", 
                                                                                        "BTS_realized_stopping_rolling_avg50",
                                                                                        "HTS"))
        
        levels(df_plot_RV_eval_QLIKE$sampling) <- levels(df_plot_RV_eval$sampling) <- c('Intensity~TTS~~~vs.~~~CTS',
                                                                                        'Realized~TTS~~~vs.~~~CTS',
                                                                                        'Realized~Stopping~Time~TTS~~~vs.~~~CTS',
                                                                                        'Intensity~BTS~~~vs.~~~CTS',
                                                                                        'Realized~BTS~~~vs.~~~CTS',
                                                                                        'Realized~Stopping~Time~BTS~~~vs.~~~CTS',
                                                                                        'HTS~~~vs.~~~CTS')
        
      } else {
        df_plot_RV_eval_QLIKE$sampling <- df_plot_RV_eval$sampling <- factor(df_plot_RV_eval$sampling, 
                                                                             levels = c("CTS",
                                                                                        "TTS_rolling_avg50",
                                                                                        "TTS_realized",
                                                                                        "TTS_realized_stopping",
                                                                                        "BTS_rolling_avg50", 
                                                                                        "BTS_realized_stopping_rolling_avg50",
                                                                                        "HTS"))
        
        levels(df_plot_RV_eval_QLIKE$sampling) <- levels(df_plot_RV_eval$sampling) <- c('CTS~~~vs.~~~Realized~BTS',
                                                                                        'Intensity~TTS~~~vs.~~~Realized~BTS',
                                                                                        'Realized~TTS~~~vs.~~~Realized~BTS',
                                                                                        'Realized~Stopping~Time~TTS~~~vs.~~~Realized~BTS',
                                                                                        'Intensity~BTS~~~vs.~~~Realized~BTS',
                                                                                        'Realized~Stopping~Time~BTS~~~vs.~~~Realized~BTS',
                                                                                        'HTS~~~vs.~~~Realized~BTS')
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
      
      ggsave(paste0("application/plots/appl_eval_RR2_MSE_aggregation_",aggregation_method,"_",choice_est,"_Baseline_",choice_baseline_sampling,".pdf"), width=9, height=6)
      
      
      
      # ## Plot points with magnitude of RMSE gains/losses as a color scale for a presentation!!!
      # ggplot(df_plot_RV_eval %>% 
      #          filter(significant_logical==T,
      #                 sampling %in% c("CTS~~~vs.~~~Realized~BTS", "HTS~~~vs.~~~Realized~BTS"))) + 
      #   geom_point(aes(x=M,
      #                  y=asset,
      #                  col=RMSE_rel), 
      #              size=3) +
      #   facet_wrap(~sampling, ncol=4, labeller=label_parsed) +
      #   scale_color_gradient2(low = "red", mid = "white", high = "black", 
      #                         space = "Lab",
      #                         labels = scales::percent) +
      #   scale_y_discrete(limits = rev) +
      #   theme_bw() +
      #   ylab("Asset") +
      #   # theme(legend.position = "right", legend.title = element_text(angle = 90)) +
      #   theme(legend.position = "right") +
      #   guides(col = guide_colourbar(barwidth = 1, barheight = 15, title="Relative RMSE\nImprovement"))
      # 
      # ggsave(paste0("application/plots/appl_eval_RR2_MSE_Presentation_aggregation_",aggregation_method,"_",choice_est,"_Baseline_",choice_baseline_sampling,".pdf"), width=6, height=6)
      # 
      
      
      
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
      
      ggsave(paste0("application/plots/appl_eval_RR2_QLIKE_aggregation_",aggregation_method,"_",choice_est,"_Baseline_",choice_baseline_sampling,".pdf"), width=9, height=6)
      
    }
  }
}
  

saveRDS(df_CompPosNeg, file = paste0(dir_base, "/application/data/df_CompPosNeg.rds"))

  



##############
# Save summary tables
# What do we print in one "row panel"? 2 different baselines?

df_CompPosNeg$sampling <- factor(df_CompPosNeg$sampling, 
                                       levels = c("CTS", 
                                                  "TTS_rolling_avg50", "TTS_realized","TTS_realized_stopping",
                                                  "BTS_rolling_avg50", "BTS_realized_rolling_avg50", "BTS_realized_stopping_rolling_avg50",
                                                  "HTS"))    
levels(df_CompPosNeg$sampling) <- c("CTS",
                                    "iTTS", "rTTS", "rsTTS",
                                    "iBTS", "rBTS", "rsBTS",
                                    "HTS")



# Loop over aggregation methods 
for (aggregation_method in aggregation_methods){
  # Loop over different estimators (RV and PVAG)
  for (choice_est in estim_set){
    # Loop over different baseline sampling schemes
    for (choice_baseline_sampling in baseline_sampling_set){

      # Print a table with amount of positive/negative losses for CTS
      df_CompPosNeg_pretty <- df_CompPosNeg %>%
        dplyr::filter(aggreg_method==aggregation_method,
                      type_estimator==choice_est,
                      baseline_sampling==choice_baseline_sampling) %>%
        rename(pos=significant_pos, neg=significant_neg) %>%
        mutate(pos = 100*pos,
               neg = 100*neg) %>%
        mutate(col_id = paste0(loss)) %>%
        select(baseline_sampling, sampling, col_id, pos, neg) %>%
        pivot_wider(names_from = col_id,
                    values_from = c(pos,neg),
                    names_vary="slowest") %>%
        arrange(sampling) 
      
      
      df_CompPosNeg_pretty %>%
        mutate(empty1=NA, empty2=NA, empty3=NA, empty4=NA) %>%
        dplyr::select(sampling, empty1,
                      pos_RMSE, neg_RMSE, empty2,
                      pos_QLIKE, neg_QLIKE) %>% 
        # , empty3,
        #               pos_RV_PAVG_RMSE, neg_RV_PAVG_RMSE, empty4,
        #               pos_RV_PAVG_QLIKE, neg_RV_PAVG_QLIKE) %>%
        xtable::xtable(digits=c(0,0,0, 0,0,0, 0,0)) %>%
        print(file=paste0("application/plots/PosNegValues_RR",aggregation_method,"_",choice_est,"_Baseline_",choice_baseline_sampling,".txt"), include.rownames=FALSE, booktabs=TRUE)  
      
    }
  }
}


