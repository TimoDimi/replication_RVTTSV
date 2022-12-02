library(multidplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(forcats)

source("sample_schemes_est.R")
source("simulations/sim_TTSV.R")
source("lambda_est.R")
source("varsigma_est.R")
source("RV_comparison.R")

dir_base <- getwd()

df_RV_full <- readRDS(file = paste0(dir_base, "/application/data/RV_est_RollingSS.rds"))

# Just plot a choice of common M values
M_set <-  c(6,13,26,78,260,390,585,780)

# Set this to either "RV" or RV_AC1" to get the different plots
choice_est <- "RV"

df_RV <- df_RV_full %>% 
  dplyr::filter(Date >= as_date("2010-01-01"), 
                Date <= as_date("2019-12-31"), 
                M %in% M_set,
                type_estimator==choice_est)
                

# Fix a baseline and a proxy. The proxy is however more important than the baseline.
# The first mutate replicates one RV value per group for the baseline (and RV_proxy_hlp)
# The second mutate call "leads" the RV_proxy_hlp by Date
df_RV_proxy <- df_RV %>%
  group_by(asset, Date) %>%
  mutate(RV_proxy_hlp = RV[M==78 & type_estimator==choice_est & sampling=="CTS"]) %>%
  ungroup() %>%
  group_by(asset, Date, M) %>%
  mutate(RV_baseline = RV[type_estimator==choice_est & sampling=="CTS"]) %>%
  ungroup() %>%
  group_by(asset, sampling, days_past, M, type_estimator) %>%
  mutate(RV_proxy = lead(RV_proxy_hlp,
                         order_by=Date))


# Create a cluster and add the essential packages and functions/variables to each worker
cluster <- new_cluster(9)
cluster %>%
  cluster_library("tidyverse") %>%
  cluster_library("tibble") %>%
  cluster_library("sandwich") %>%
  cluster_copy("RV_comparison")


# Actual evaluation in parallel
set.seed(1)
df_RV_eval <- df_RV_proxy %>%
  group_by(asset, sampling, days_past, M, type_estimator) %>%
  partition(cluster) %>% 
  summarize(RV_comparison(RV = RV,
                          RV_baseline = RV_baseline,
                          IV_proxy = RV_proxy,
                          B=1000),
            n=n()) %>%
  collect()



# ggplot of the results
sig_level <- 0.05

df_plot_RV_eval <- df_RV_eval %>%
  dplyr::filter(type_estimator==choice_est) %>%
  mutate(sign = ifelse(mean_loss_diff > 0, "positive", "negative"),
         RMSE_rel=pmax(pmin(RMSE_rel,0.2),-0.2),
         significant_logical = as.logical(p_val <= sig_level)) %>%
  dplyr::filter(M %in% M_set,
                days_past %in% c(NA,5,20,250),
                !sampling %in% c("CTS")) 

# Improve the facet labels
df_plot_RV_eval_pretty_hlp <- df_plot_RV_eval %>%
  mutate(asset = factor(asset),
         M = factor(M),
         sampling_pretty = factor(paste0(sampling,"_",days_past))) %>%
  group_by(sampling_pretty) %>%
  mutate(significant_pos = mean(p_val <= sig_level & RMSE_rel>0),
         significant_neg = mean(p_val <= sig_level & RMSE_rel<0),
         significant_label = paste0(100*round(significant_pos,2),
                                    "pos_",
                                    100*round(significant_neg,2),
                                    "neg_"),
         significant_label = factor(significant_label)) %>%
  ungroup()


df_plot_RV_eval_pretty <- df_plot_RV_eval_pretty_hlp

levels(df_plot_RV_eval_pretty$sampling_pretty) 
levels(df_plot_RV_eval_pretty$sampling_pretty) <- c('BTS:~(C)~Same-day~sampling',
                                                    expression(BTS:~(B)~Past~avg~~Delta~"="~5),
                                                    expression(BTS:~(B)~Past~avg~~Delta~"="~20),
                                                    expression(BTS:~(B)~Past~avg~~Delta~"="~250),
                                                    'TTS:~(C)~Same-day~sampling',
                                                    expression(TTS:~(B)~Past~avg~~Delta~"="~5),
                                                    expression(TTS:~(B)~Past~avg~~Delta~"="~20),
                                                    expression(TTS:~(B)~Past~avg~~Delta~"="~250))

# Manual selection for the standard RV estimator
# df_plot_RV_eval_pretty_hlp %>% filter(asset == "AA", M==78) %>% select(sampling, days_past, significant_label)
levels(df_plot_RV_eval_pretty_hlp$significant_label)
levels(df_plot_RV_eval_pretty$significant_label) <- c(expression(29~"%"~positive~~~4~"%"~negative),
                                                      expression(27~"%"~positive~~~4~"%"~negative),
                                                      expression(28~"%"~positive~~~8~"%"~negative),
                                                      expression(25~"%"~positive~~~4~"%"~negative),
                                                      expression(34~"%"~positive~~~6~"%"~negative),
                                                      expression(32~"%"~positive~~~6~"%"~negative),
                                                      expression(29~"%"~positive~~~6~"%"~negative),
                                                      expression(34~"%"~positive~~~7~"%"~negative))

# # Manual selection for the RV AC(1) estimator
# # df_plot_RV_eval_pretty_hlp %>% filter(asset == "AA", M==78) %>% select(sampling, days_past, significant_label) %>% arrange(sampling,days_past)  
# levels(df_plot_RV_eval_pretty_hlp$significant_label)
# levels(df_plot_RV_eval_pretty$significant_label) <- c(expression(28~"%"~positive~~~7~"%"~negative),
#                                                       expression(33~"%"~positive~~~5~"%"~negative),
#                                                       expression(28~"%"~positive~~~6~"%"~negative),
#                                                       expression(24~"%"~positive~~~12~"%"~negative),
#                                                       expression(28~"%"~positive~~~8~"%"~negative),
#                                                       expression(23~"%"~positive~~~10~"%"~negative),
#                                                       expression(31~"%"~positive~~~9~"%"~negative))


## Plot points with magnitude of RMSE gains/losses as a color scale!
ggplot(df_plot_RV_eval_pretty %>% filter(significant_logical==T)) + 
  geom_point(aes(x=M,
                 y=asset,
                 col=RMSE_rel), 
             size=3) +
  facet_wrap(~sampling_pretty + significant_label, ncol=4, labeller=label_parsed) +
  scale_color_gradient2(low = "red", mid = "white", high = "black", 
                        space = "Lab",
                        labels = scales::percent) +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  ylab("Asset") +
  theme(legend.position = "bottom") +
  guides(col = guide_colourbar(barwidth = 15, barheight = 1, title="Relative RMSE Improvement    "))

ggsave(paste0("application/plots/appl_eval_",choice_est,".pdf"), width=8, height=10)




