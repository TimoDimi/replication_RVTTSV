library(tidyverse)
library(grid)
library(gridExtra)
library(scales) 
library(ggh4x)


# Read simulation results with individual runs:
# file list RVs:
data_files_RVest <- list.files(path="SCS-FUCHS/data_sim/data_rds_20250509/", pattern="sim_RVest", full.names=TRUE, recursive=FALSE)

res_df_raw <- tibble()
for (file in data_files_RVest){
  df_tmp <- readRDS(file)
  res_df_raw <- bind_rows(res_df_raw, df_tmp)
}


# Set negative RV_PAVG estimates to eps_RV
eps_RV <- 10^(-10)
res_df <- res_df_raw %>%
  mutate(RV = pmax(RV, eps_RV))


# Use rIV for the Hawkes-type process for approximation reasons!
res_df <- res_df %>%
  mutate(IV = case_when(
    leverage_setting=="Hawkes" ~ rIV,
    .default = IV))


# Check for similar (r)IV values
res_df %>% 
  filter(sampling=="CTS", M==78, sd_eps==0) %>%
  group_by(type_estimator, lambda_setting, noise_setting, leverage_setting, sd_eps) %>%
  reframe(IV_mean = mean(IV),
          rIV_mean = mean(rIV)) %>%
  print(n=100)





# Compute the average bias and MSE over the simulation replications
df_est_nonHTS <- res_df %>%
  filter(sampling != "HTS" & !str_detect(sampling, "stopping")) %>%
  group_by(sampling, M, type_estimator, lambda_setting, noise_setting, leverage_setting, sd_eps) %>%
  summarize(bias=mean(RV-IV, na.rm=T),
            bias_rel=mean((RV-IV)/IV, na.rm=T),
            MSE=mean((RV-IV)^2, na.rm=T),
            RMSE_rel=sqrt(mean((RV-IV)^2, na.rm=T))/mean(IV, na.rm=T),
            QLIKE=mean(IV/RV - log(IV/RV) - 1, na.rm=T),
            n=n())


# Compute the average bias and MSE over the simulation replications
df_est_stopping <- res_df %>%
  filter(str_detect(sampling, "stopping")) %>%
  group_by(sampling, n_aggregate, type_estimator, lambda_setting, noise_setting, leverage_setting, sd_eps) %>%
  summarize(bias=mean(RV-IV, na.rm=T),
            bias_rel=mean((RV-IV)/IV, na.rm=T),
            MSE=mean((RV-IV)^2, na.rm=T),
            RMSE_rel=sqrt(mean((RV-IV)^2, na.rm=T))/mean(IV, na.rm=T),
            QLIKE=mean(IV/RV - log(IV/RV) - 1, na.rm=T),
            M=mean(M),
            n=n())



# Compute the average bias and MSE over the simulation replications
df_est_HTS <- res_df %>%
  filter(sampling == "HTS") %>%
  filter(M < 5000) %>% # Only use instances where "the full day was sampled"
  group_by(sampling, delta, type_estimator, lambda_setting, noise_setting, leverage_setting, sd_eps) %>%
  summarize(bias=mean(RV-IV, na.rm=T),
            bias_rel=mean((RV-IV)/IV, na.rm=T),
            MSE=mean((RV-IV)^2, na.rm=T),
            RMSE_rel=sqrt(mean((RV-IV)^2, na.rm=T))/mean(IV, na.rm=T),
            QLIKE=mean(IV/RV - log(IV/RV) - 1, na.rm=T),
            M = mean(M),
            M_expected = mean(M_expected),
            n=n())

# Bind together
df_est <- bind_rows(df_est_nonHTS, df_est_stopping, df_est_HTS)


###### Cosmetic changes for the plots

# recode sampling scheme names
df_est2 <- df_est %>% 
  mutate(sampling_f = factor(sampling),
         sampling_type = forcats::fct_recode(sampling_f, 
                                             BTS="BTS_true",
                                             BTS="BTS_realized_true",
                                             BTS="BTS_realized_rolling_avg50",
                                             BTS="BTS_rolling_avg50",
                                             BTS="BTS_realized_stopping_rolling_avg50",
                                             CTS="CTS",
                                             TTS="TTS_true",
                                             TTS="TTS_realized",
                                             TTS="TTS_realized_stopping",
                                             HTS="HTS"),
         sampling_name = forcats::fct_recode(sampling_f, 
                                             BTS="BTS_true",
                                             rBTS_oracle="BTS_realized_true",
                                             rBTS="BTS_realized_rolling_avg50",
                                             iBTS="BTS_rolling_avg50",
                                             "rBTS stopping"="BTS_realized_stopping_rolling_avg50",
                                             CTS="CTS",
                                             "iTTS oracle"="TTS_true",
                                             rTTS="TTS_realized",
                                             "rTTS stopping"="TTS_realized_stopping",
                                             HTS="HTS"),
         sampling_est = forcats::fct_recode(sampling_f, 
                                            true="CTS",
                                            true="TTS_true",
                                            true="BTS_true",
                                            # intensity_rolling="TTS_rolling_avg50",
                                            intensity_rolling="BTS_rolling_avg50",
                                            realized_true="BTS_realized_true",
                                            realized="TTS_realized",
                                            realized="BTS_realized_rolling_avg50",
                                            realized="HTS",
                                            realized_stopping="BTS_realized_stopping_rolling_avg50",
                                            realized_stopping="TTS_realized_stopping")) 


# Artificially add CTS as "past", "realized" and "rolling" as well
df_est3 <- bind_rows(df_est2, 
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="realized_true"),
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="realized"),
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="rolling"),
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="realized_stopping"),
                     df_est2 %>% filter(sampling=="TTS_realized") %>% mutate(sampling_est="realized_true")) %>%
  mutate(sampling_est = factor(sampling_est),
         noise_setting = factor(noise_setting))


# Recode (reorder) factor "sampling_type"
df_est3$sampling_type <- recode_factor(df_est3$sampling_type,
                                       CTS="CTS",
                                       TTS="TTS",
                                       BTS="BTS",
                                       HTS="HTS")

# Recode (reorder) factor "sampling_name"
df_est3$sampling_name <- recode_factor(df_est3$sampling_name,
                                       CTS="CTS",
                                       rTTS="rTTS",
                                       rBTS="rBTS",
                                       HTS="HTS")



# Recode factor "type_estimator"
df_est3$type_estimator <- recode_factor(df_est3$type_estimator,
                                        RV="RV",
                                        RV_PAVG="RV~PAVG")
levels(df_est3$type_estimator) <- c('RV',
                                    'Preaveraged~RV')


# Recode factor "noise_setting_name"
df_est3$noise_setting <- recode_factor(df_est3$noise_setting,
                                       iid="iid", ARMA="ARMA", ARMAdiurnal="ARMA-diurnal")
levels(df_est3$noise_setting) <- c("i.i.d. noise",
                                   "ARMA noise",
                                   "diurnal ARMA noise")


# Recode factor "noise_setting_name"
df_est3$leverage_setting <- recode_factor(df_est3$leverage_setting,
                                          independent="independent", Hawkes="Hawkes")
levels(df_est3$leverage_setting) <- c("Independent TTSV process",
                                      "Hawkes-type TTSV process")

# Recode factor "lambda_setting"
df_est3$lambda_setting <- factor(df_est3$lambda_setting)
levels(df_est3$lambda_setting) <- c("2000 Ticks per day", "8000 Ticks per day", "32000 Ticks per day") 


# Recode factor "sd_eps"
df_est3$sd_eps <- factor(df_est3$sd_eps)
levels(df_est3$sd_eps) <- c("0% Noise", "25% Noise", "50% Noise", "100% Noise", "200% Noise") 






################################################################################
# (1) Plot Bias/RMSEs of Hawkes/TTSV against noise level [fix lambda=4000, noise="iid]
################################################################################
df_plot <- df_est3 %>% 
  dplyr::filter(type_estimator=="RV", 
                noise_setting=="i.i.d. noise", 
                lambda_setting=="8000 Ticks per day",
                sd_eps %in% c("0% Noise", "25% Noise", "50% Noise", "100% Noise"),
                M>=10,
                M<5000,
                #sampling %in% c("TTS_realized", "TTS_realized_stopping", "BTS_realized_rolling_avg50", "BTS_realized_stopping_rolling_avg50")
                sampling %in% c("CTS", "BTS_realized_rolling_avg50", "TTS_realized", "HTS")
                )

# 1a: Bias
ggplot(df_plot) +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(x=M, y=bias_rel, col=sampling_name, linetype=sampling_name)) +
  # scale_linetype_manual(values=c(4,2,1)) +
  ggh4x::facet_grid2(as.factor(leverage_setting)~sd_eps, scales="free_y", independent="y") +
  # facet_wrap(as.factor(leverage_setting)~sd_eps, scales="free_y", nrow=2) +
  # scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1560),
                     trans='log2') +
  coord_cartesian(xlim=c(26,1560)) + 
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  ylab("Relative Bias") +
  xlab("Amount of samples: M") +
  scale_y_continuous(labels = percent) +  # Convert y-axis to percentages
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR2/Bias_NoiseXProcess.pdf", width=9, height=5)


# 1b: RMSE
ggplot(df_plot) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_name, linetype=sampling_name)) +
  # scale_linetype_manual(values=c(4,2,1)) +
  # ggh4x::facet_grid2(as.factor(leverage_setting)~sd_eps, scales="free_y", independent="y") +
  facet_grid(as.factor(leverage_setting)~sd_eps) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1560),
                     trans='log2') +
  coord_cartesian(xlim=c(26,1560)) + 
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  ylab("Relative RMSE") +
  xlab("Amount of samples: M") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR2/RMSE_NoiseXProcess.pdf", width=9, height=5.4)



# 1b PRESENTATIONS: RMSE
ggplot(df_plot %>% filter(leverage_setting=="Independent TTSV process",
                          sd_eps!="25% Noise",
                          sampling != "TTS_realized")) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_name, linetype=sampling_name)) +
  # scale_linetype_manual(values=c(4,2,1)) +
  # ggh4x::facet_grid2(as.factor(leverage_setting)~sd_eps, scales="free_y", independent="y") +
  facet_grid(as.factor(leverage_setting)~sd_eps) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1560),
                     trans='log2') +
  coord_cartesian(xlim=c(26,1560)) + 
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  ylab("Relative RMSE") +
  xlab("Amount of samples: M") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR2/RMSE_NoiseXProcess_Presentation.pdf", width=7, height=3.5)



################################################################################
# (1c) Compare bias to approximations!
################################################################################

df_bias_approx <- readRDS(file = paste0("simulations/data/bias_approx_20250514.rds")) %>%
  select(-bias_approx) %>%
  pivot_longer(cols = starts_with("bias_approx"), values_to="bias_approx", names_to="bias_term") 

# Recode bias_term factor
df_bias_approx <- df_bias_approx %>%
  mutate(bias_term = factor(bias_term, 
                            levels = c("bias_approx_covterm", "bias_approx_varterm"),
                            labels = c("Covariance Term", "Variance Term")
         ))


# Recode factor "sd_eps"
df_bias_approx$sd_eps <- factor(df_bias_approx$sd_eps)
levels(df_bias_approx$sd_eps) <- c("25% Noise", "100% Noise") 


df_bias_approx_plot <- full_join(df_bias_approx %>% 
                                   mutate(sampling = recode(sampling, "BTS_realized_rolling_avg5" = "BTS_realized_rolling_avg50")),
                                 df_plot %>% 
                                   ungroup() %>% 
                                   dplyr::filter(leverage_setting == "Independent TTSV process") %>%
                                   dplyr::select(sampling, M, sd_eps, bias, bias_rel)) %>%
  mutate(sampling = factor(sampling),
         sampling_name = forcats::fct_recode(sampling, 
                                             CTS="CTS",
                                             rTTS="TTS_realized",
                                             rBTS="BTS_realized_rolling_avg50",
                                             HTS="HTS"))

# Recode (reorder) factor "sampling_name"
df_bias_approx_plot$sampling_name <- recode_factor(df_bias_approx_plot$sampling_name,
                                                   CTS="CTS",
                                                   rTTS="rTTS",
                                                   rBTS="rBTS",
                                                   HTS="HTS")



# Approximate values for HTS because of differently shaped M values...
df_bias_approx_plot2 <- df_bias_approx_plot %>%
  group_by(sampling_name, sd_eps) %>%
  mutate(bias = zoo::na.approx(bias, x=M, na.rm = FALSE)) %>%
  ungroup()


ggplot(df_bias_approx_plot2 %>%
         filter(sd_eps %in% c("25% Noise", "100% Noise"))) +
  geom_hline(aes(yintercept = 0)) +
  geom_area(aes(x = M, y = bias_approx, fill = bias_term), alpha = 0.7) +
  geom_line(aes(x = M, y = bias, color = sampling_name)) +
  ggh4x::facet_grid2(sd_eps ~ sampling_name, scales = "free_y", independent = "none") +
  scale_x_continuous(
    breaks = c(26, 78, 390, 1560),
    trans = 'log2'
  ) +
  coord_cartesian(xlim = c(26, 1560)) +
  scale_fill_manual(name = "Bias approximation", values = c("gray20", "gray70")) +
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  ylab("Absolute Bias") +
  xlab("Amount of samples: M") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",   # stack legends vertically
    aspect.ratio = 1,
    legend.key.width = unit(1, "cm"),
    panel.spacing.y = unit(1, "lines")
  ) +
  guides(
    # color = guide_legend(title = "Sampling scheme", nrow = 1, order = 1),
    color = "none",
    fill = guide_legend(title = "Bias approximation", nrow = 1, order = 2)
  )

ggsave("simulations/plots_RR2/Bias_Absolute_NoiseXProcess.pdf", width=9, height=5)



# For a presentation
ggplot(df_bias_approx_plot2 %>%
         filter(sd_eps %in% c("100% Noise"),
                sampling != "TTS_realized")) +
  geom_hline(aes(yintercept = 0)) +
  geom_area(aes(x = M, y = bias_approx, fill = bias_term), alpha = 0.7) +
  geom_line(aes(x = M, y = bias, color = sampling_name)) +
  ggh4x::facet_grid2(sd_eps ~ sampling_name, scales = "free_y", independent = "none") +
  scale_x_continuous(
    breaks = c(26, 78, 390, 1560),
    trans = 'log2'
  ) +
  coord_cartesian(xlim = c(26, 1560)) +
  scale_fill_manual(name = "Bias approximation", values = c("gray20", "gray70")) +
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  ylab("Absolute Bias") +
  xlab("Amount of samples: M") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",   # stack legends vertically
    aspect.ratio = 1,
    legend.key.width = unit(1, "cm"),
    panel.spacing.y = unit(1, "lines")
  ) +
  guides(
    color = "none",  # remove the color legend
    fill = guide_legend(title = "Bias approximation", nrow = 1, order = 2)
  )

ggsave("simulations/plots_RR2/Bias_Absolute_NoiseXProcess_Presentation.pdf", width=7, height=3.5)








################################################################################
# (2)  Plot Bias/RMSEs of amount of observations against noise level [fix "Hawkes" and noise="ARMA-diurnal]
################################################################################

df_plot2 <- df_est3 %>% 
  dplyr::filter(type_estimator=="RV", 
                noise_setting=="i.i.d. noise", 
                leverage_setting=="Hawkes-type TTSV process",
                sd_eps %in% c("0% Noise", "25% Noise", "50% Noise", "100% Noise"),
                M>=10,
                M<5000,
                sampling %in% c("CTS", "BTS_realized_rolling_avg50", "TTS_realized", "HTS")
  )



# 2b: RMSE
ggplot(df_plot2) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_name, linetype=sampling_name)) +
  # scale_linetype_manual(values=c(4,2,1)) +
  # ggh4x::facet_grid2(as.factor(leverage_setting)~sd_eps, scales="free_y", independent="y") +
  facet_grid(lambda_setting~sd_eps) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1560),
                     trans='log2') +
  coord_cartesian(xlim=c(26,1560)) + 
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  ylab("Relative RMSE") +
  xlab("Amount of samples: M") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR2/RMSE_NoiseXTicksperday.pdf", width=9, height=7.5)








################################################################################
# (3)  Plot Bias/RMSEs of noise specification against noise level [fix "Hawkes" and lamdba=4000]
################################################################################

df_plot3 <- df_est3 %>% 
  dplyr::filter(type_estimator=="RV", 
                lambda_setting=="8000 Ticks per day",
                leverage_setting=="Hawkes-type TTSV process",
                sd_eps %in% c("0% Noise", "25% Noise", "50% Noise", "100% Noise"),
                M>=10,
                M<5000,
                sampling %in% c("CTS", "BTS_realized_rolling_avg50", "TTS_realized", "HTS")
  )



# 3b: RMSE
ggplot(df_plot3) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_name, linetype=sampling_name)) +
  # scale_linetype_manual(values=c(4,2,1)) +
  # ggh4x::facet_grid2(as.factor(leverage_setting)~sd_eps, scales="free_y", independent="y") +
  facet_grid(noise_setting~sd_eps) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1560),
                     trans='log2') +
  coord_cartesian(xlim=c(26,1560)) + 
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  ylab("Relative RMSE") +
  xlab("Amount of samples: M") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR2/RMSE_NoiseXNoiseprocess.pdf", width=9, height=7.5)





################################################################################
# (4) TODO: MAYBE FOR SUPPLEMENT: Compare estimation variants of sampling schemes in standard setting
################################################################################
df_plot4 <- df_est3 %>% 
  dplyr::filter(type_estimator=="RV", 
                noise_setting=="diurnal ARMA noise", 
                lambda_setting=="8000 Ticks per day",
                leverage_setting=="Hawkes-type TTSV process",
                sd_eps %in% c("0% Noise", "25% Noise", "50% Noise", "100% Noise"),
                M>=10,
                M<5000,
                sampling %in% c("TTS_true", "TTS_realized", "BTS_realized_rolling_avg50", "BTS_realized_true", "BTS_true", "BTS_rolling_avg50")
  )



# 4b: RMSE
ggplot(df_plot4) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_est, linetype=sampling_est)) +
  scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  scale_color_discrete(labels = c("Realized (estimated)", "Realized (true)", "Intensity (estimated)", "Intensity (true)")) +
  scale_linetype_discrete(labels = c("Realized (estimated)", "Realized (true)", "Intensity (estimated)", "Intensity (true)")) +
  # scale_linetype_manual(values=c(4,2,1)) +
  # ggh4x::facet_grid2(as.factor(leverage_setting)~sd_eps, scales="free_y", independent="y") +
  facet_grid(sampling_type~sd_eps) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1560),
                     trans='log2') +
  coord_cartesian(xlim=c(26,1560)) + 
  ylab("Relative RMSE") +
  xlab("Amount of samples: M") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots_RR2/RMSE_NoiseXEstimationSampling.pdf", width=9, height=5.4)






################################################################################
# (5) MAYBE FOR SUPPLEMENT: Compare estimation variants of sampling schemes in standard setting
################################################################################
df_plot5 <- df_est3 %>% 
  dplyr::filter(type_estimator=="RV", 
                noise_setting=="diurnal ARMA noise", 
                lambda_setting=="8000 Ticks per day",
                leverage_setting=="Hawkes-type TTSV process",
                sd_eps %in% c("0% Noise", "25% Noise", "50% Noise", "100% Noise"),
                M>=10,
                M<5000,
                sampling_est != "realized_true", # ToDo: Get rid later on!
                sampling %in% c("BTS_realized_rolling_avg50", "BTS_realized_stopping_rolling_avg50", "TTS_realized", "TTS_realized_stopping")
  )



# 5b: RMSE
ggplot(df_plot5) +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_est, linetype=sampling_est)) +
  scale_color_discrete(labels = c("Realized stopping time", expression(paste("Realized (using information in ", Phi(T), ")")))) +
  scale_linetype_discrete(labels = c("Realized stopping time", expression(paste("Realized (using information in ", Phi(T), ")")))) +
  # scale_linetype_manual(values=c(4,2,1)) +
  # ggh4x::facet_grid2(as.factor(leverage_setting)~sd_eps, scales="free_y", independent="y") +
  facet_grid(sampling_type~sd_eps) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(26,78,390,1560),
                     trans='log2') +
  coord_cartesian(xlim=c(26,1560)) + 
  #scale_colour_manual(values = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF")) +
  ylab("Relative RMSE") +
  xlab("Amount of samples: M") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1,
        legend.key.width=unit(1,"cm"),
        panel.spacing.y = unit(1, "lines")) +
  guides(color=guide_legend(title="Sampling scheme"),
         linetype=guide_legend(title="Sampling scheme"))


ggsave("simulations/plots_RR2/RMSE_NoiseXStoppingTimeSampling.pdf", width=9, height=5.5)

