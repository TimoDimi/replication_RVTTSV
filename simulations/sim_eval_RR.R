library(tidyverse)
library(grid)
library(gridExtra)

# Read simulation results with individual runs:
# file list RVs:
data_files_RVest <- list.files(path="simulations/data/sim_files_20231125", pattern="sim_RVest", full.names=TRUE, recursive=FALSE)

res_df <- tibble()
for (file in data_files_RVest){
  df_tmp <- readRDS(file)
  res_df <- bind_rows(res_df, df_tmp)
}


# Set negative RV_PAVG estimates to eps_RV
eps_RV <- 10^(-8)
res_df <- res_df %>%
  mutate(RV = pmax(RV, eps_RV))

# Compute the average bias and MSE over the simulation replications
df_est <- res_df %>%
  group_by(sampling, M, type_estimator, noise_setting, leverage_setting) %>%
  summarize(bias=mean(RV-IV, na.rm=T),
            bias_rel=mean((RV-IV)/IV, na.rm=T),
            MSE=mean((RV-IV)^2, na.rm=T),
            RMSE_rel=sqrt(mean((RV-IV)^2, na.rm=T))/mean(IV, na.rm=T),
            QLIKE=mean(IV/RV - log(IV/RV) - 1, na.rm=T),
            n=n())



# recode sampling scheme names
df_est2 <- df_est %>% 
  mutate(sampling_f = factor(sampling),
         sampling_type = forcats::fct_recode(sampling_f, 
                                             BTS="BTS_true",
                                             BTS="BTS_daily",
                                             BTS="BTS_realized_true",
                                             BTS="BTS_realized_rolling_avg50",
                                             BTS="BTS_rolling_avg50",
                                             CTS="CTS",
                                             TTS="TTS_true",
                                             TTS="TTS_daily",
                                             TTS="TTS_realized",
                                             TTS="TTS_rolling_avg50"),
         sampling_est = forcats::fct_recode(sampling_f, 
                                            true="BTS_true",
                                            daily="BTS_daily",
                                            realized_true="BTS_realized_true",
                                            realized_rolling="BTS_realized_rolling_avg50",
                                            rolling="BTS_rolling_avg50",
                                            true="CTS",
                                            true="TTS_true",
                                            daily="TTS_daily",
                                            realized_true="TTS_realized",
                                            rolling="TTS_rolling_avg50")) 

# Artificially add CTS as "past", "realized" and "rolling" as well
df_est3 <- bind_rows(df_est2, 
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="daily"),
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="realized_true"),
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="realized_rolling"),
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="rolling"),
                     df_est2 %>% filter(sampling=="TTS_realized") %>% mutate(sampling_est="realized_rolling")) %>%
  mutate(sampling_est = factor(sampling_est),
         noise_setting = factor(noise_setting))


# Recode (reorder) factor "sampling_type"
df_est3$sampling_type <- recode_factor(df_est3$sampling_type,
                                       CTS="CTS",
                                       TTS="TTS",
                                       BTS="BTS")


#####  Cosmetic changes!!!
levels(df_est3$type_estimator) <- c('RV',
                                  'Preaveraged~RV')




df_est3$sampling_est <- recode_factor(df_est3$sampling_est, true="true", rolling="rolling", daily="daily", realized_true="realized_true", realized_rolling="realized_rolling") 
levels(df_est3$sampling_est) <- c('True~intensities',
                                  'Rolling~intensities',
                                  'Same-day~estimate',
                                  'Realized~(true~varsigma)',
                                  'Realized~(rolling~varsigma)')

# Different names formatting for the legend 
df_est3$sampling_est_legend <- df_est3$sampling_est
levels(df_est3$sampling_est_legend) <- c('True intensities',
                                  'Rolling intensities',
                                  'Same-day estimate',
                                  'Realized (true varsigma)',
                                  'Realized (rolling varsigma)')

# Recode factor "noise_setting_name"
df_est3$noise_setting <- recode_factor(df_est3$noise_setting,
                                            no="no", iid="iid", ARMA="ARMA", ARMAdiurnal="ARMA-diurnal")
levels(df_est3$noise_setting) <- c("No~noise",
                                        "i.i.d.~noise",
                                        "ARMA~noise",
                                        "diurnal~ARMA~noise")


# Recode factor "type_estimator"
df_est3$type_estimator <- recode_factor(df_est3$type_estimator,
                                       RV="RV",
                                       RV_PAVG="RV~PAVG")



################################################################################
###       Plot Biases
################################################################################

# (1) Plot relative bias 
ggplot(df_est3 %>% 
         dplyr::filter(sampling_est %in% c("Rolling~intensities", "Realized~(rolling~varsigma)"),
                       leverage_setting=="independent", 
                       noise_setting!="ARMA~noise", 
                       M>=26,
         )) +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(x=M, y=bias_rel, col=sampling_type, linetype=sampling_type)) +
  scale_linetype_manual(values=c(4,2,1)) +
  facet_grid(noise_setting~type_estimator+sampling_est, scales="free", labeller=label_parsed) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=c(26,78,390,1170, 4680),
                     trans='log2') +
  scale_colour_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  ylab("Relative Bias") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR/Sim_Bias_ind.pdf", width=8, height=6.5)




# (2) Plot relative bias for leverage process
ggplot(df_est3 %>% 
         dplyr::filter(sampling_est %in% c("Rolling~intensities", "Realized~(rolling~varsigma)"),
                       leverage_setting=="leverage", 
                       noise_setting!="ARMA~noise", 
                       M>=26,
                       sampling_est!="Same-day~estimate"
         )) +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(x=M, y=bias_rel, col=sampling_type, linetype=sampling_type)) +
  scale_linetype_manual(values=c(4,2,1)) +
  facet_grid(noise_setting~type_estimator+sampling_est, scales="free", labeller=label_parsed) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=c(26,78,390,1170, 4680),
                     trans='log2') +
  scale_colour_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  ylab("Relative Bias") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR/Sim_Bias_leverage.pdf", width=8, height=6.5)





################################################################################
###       Plot MSEs
################################################################################


# (1) Plot relative RMSEs of independent processes, estimation variant in facets
ggplot(df_est3 %>% 
         dplyr::filter(type_estimator=="RV", 
                       leverage_setting=="independent", 
                       M>=26,
                       noise_setting!="ARMA~noise", 
                       sampling_est!="Same-day~estimate")) +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_type, linetype=sampling_type)) +
  scale_linetype_manual(values=c(4,2,1)) +
  facet_grid(noise_setting~sampling_est, labeller=label_parsed) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1170, 4680),
                     trans='log2') +
  scale_colour_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  ylab("Relative RMSE") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR/Sim_MSE_RV_ind_compareSampling.pdf", width=8, height=6.5)



# (2) Plot MSEs of independent processes, sampling types in facets
p <- ggplot(df_est3 %>% 
              dplyr::filter(leverage_setting=="independent", 
                            M>=26,
                            sampling_type!="CTS",
                            sampling_est!="Same-day~estimate",
                            noise_setting!="ARMA~noise") %>%
              dplyr::filter(!(sampling_f == "TTS_realized" & sampling_est =="Realized~(rolling~varsigma)"))   # Don't display TTS_realized in the artificial "rolling/estimated" form
) +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_est_legend, linetype=sampling_est_legend)) +
  scale_linetype_manual(values=c(4,2,1,6)) +
  facet_grid(noise_setting ~ type_estimator+sampling_type, scales="free", labeller=label_parsed) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1170, 4680),
                     trans='log2') +
  ylab("Relative RMSE") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm")) +
  guides(color=guide_legend(title="Sampling"),
         linetype=guide_legend(title="Sampling"))

p

# Manually insert irregular spaces between facets
p_new = ggplot_gtable(ggplot_build(p))
p_new$widths[8] = 2*p_new$widths[8]
p_new3 <- arrangeGrob(p_new) 

ggsave("simulations/plots_RR/Sim_MSE_ind_compareSamplingEstimation.pdf", plot=p_new3, width=8, height=6.5)




# (3) Plot MSEs of ind and leverage processes! sampling types in facets
p <- ggplot(df_est3 %>% 
              dplyr::filter(type_estimator=="RV",
                            M>=26,
                            sampling_type!="CTS",
                            sampling_est!="Same-day~estimate",
                            noise_setting!="ARMA~noise") %>%
              dplyr::filter(!(sampling_f == "TTS_realized" & sampling_est =="Realized~(rolling~varsigma)"))   # Don't display TTS_realized in the artificial "rolling/estimated" form
) +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_est_legend, linetype=sampling_est_legend)) +
  scale_linetype_manual(values=c(4,2,1,6)) +
  facet_grid(noise_setting ~ leverage_setting+sampling_type, scales="free", labeller=label_parsed) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1170, 4680),
                     trans='log2') +
  ylab("Relative RMSE") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm")) +
  guides(color=guide_legend(title="Sampling"),
         linetype=guide_legend(title="Sampling"))

# Manually insert irregular spaces between facets
p_new = ggplot_gtable(ggplot_build(p))
p_new$widths[8] = 2*p_new$widths[8]
# p_new$heights[11] = 3*p_new$heights[11]
p_new3 <- arrangeGrob(p_new) 
plot(p_new3)

ggsave("simulations/plots_RR/Sim_MSE_RV_compareSamplingEstimation.pdf", plot=p_new3, width=8, height=6.5)




# (4) Plot MSEs of ind and leverage processes! sampling types in facets
p <- ggplot(df_est3 %>% 
              dplyr::filter(type_estimator=="RV~PAVG",
                            M>=26,
                            sampling_type!="CTS",
                            sampling_est!="Same-day~estimate",
                            noise_setting!="ARMA~noise") %>%
              dplyr::filter(!(sampling_f == "TTS_realized" & sampling_est =="Realized~(rolling~varsigma)"))   # Don't display TTS_realized in the artificial "rolling/estimated" form
) +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_est_legend, linetype=sampling_est_legend)) +
  scale_linetype_manual(values=c(4,2,1,6)) +
  facet_grid(noise_setting ~ leverage_setting+sampling_type, scales="free", labeller=label_parsed) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1170, 4680),
                     trans='log2') +
  ylab("Relative RMSE") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm")) +
  guides(color=guide_legend(title="Sampling"),
         linetype=guide_legend(title="Sampling"))

# Manually insert irregular spaces between facets
p_new = ggplot_gtable(ggplot_build(p))
p_new$widths[8] = 2*p_new$widths[8]
p_new3 <- arrangeGrob(p_new) 
plot(p_new3)

ggsave("simulations/plots_RR/Sim_MSE_RVPAVG_compareSamplingEstimation.pdf", plot=p_new3, width=8, height=6.5)



