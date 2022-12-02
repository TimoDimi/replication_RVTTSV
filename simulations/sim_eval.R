library(tidyverse)
library(grid)
library(gridExtra)


# Read simulation results
res_df <- readRDS(file = "simulations/data/sim_20221115.rds")

# Compute the average bias and MSE
df_est <- res_df %>%
  group_by(sampling, M, type_estimator, noise_setting_name, sd_noise, MAcoeff_noise) %>%
  summarize(bias=mean(RV-IV, na.rm=T),
            bias_rel=mean((RV-IV)/IV, na.rm=T),
            MSE=mean((RV-IV)^2, na.rm=T),
            RMSE_rel=sqrt(mean((RV-IV)^2, na.rm=T))/mean(IV, na.rm=T),
            QLIKE=mean(IV/RV - log(IV/RV) - 1, na.rm=T),
            n=n())

df_est2 <- df_est %>% 
  mutate(sampling_f = factor(sampling),
         sampling_type = forcats::fct_recode(sampling_f, 
                                             BTS="BTS_true",
                                             BTS="BTS_daily",
                                             BTS="BTS_past",
                                             BTS="BTS_rolling1",
                                             CTS="CTS",
                                             TTS="TTS_true",
                                             TTS="TTS_daily",
                                             TTS="TTS_past",
                                             TTS="TTS_rolling1"),
         sampling_est = forcats::fct_recode(sampling_f, 
                                            true="BTS_true",
                                            daily="BTS_daily",
                                            past="BTS_past",
                                            rolling1="BTS_rolling1",
                                            true="CTS",
                                            true="TTS_true",
                                            daily="TTS_daily",
                                            past="TTS_past",
                                            rolling1="TTS_rolling1")) 

# Artificially add CTS as "past" and "daily" as well
df_est3 <- bind_rows(df_est2, 
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="daily"),
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="past"),
                     df_est2 %>% filter(sampling=="CTS") %>% mutate(sampling_est="rolling1")) %>%
  mutate(sampling_est = factor(sampling_est),
         noise_setting_name = factor(noise_setting_name))


# Recode factor "sampling_type"
df_est3$sampling_type <- recode_factor(df_est3$sampling_type,
                                       CTS="CTS",
                                       TTS="TTS",
                                       BTS="BTS")

# Set factor levels to obtain nice facet captions, not sure how to do it with "recode_factor" and expression()
df_est3$sampling_est <- recode_factor(df_est3$sampling_est, true="true", past="past", rolling1="rolling1", daily="daily")
levels(df_est3$sampling_est) <- c('(A)~~True~sampling',
                                  expression((B)~~Past~average~~~Delta~"="~250),
                                  expression((B)~~Past~day~~~Delta~"="~1),
                                  '(C)~~Daily~sampling')



# Recode factor "noise_setting_name"
df_est3$noise_setting_name <- recode_factor(df_est3$noise_setting_name, 
                                            no="no", low_iid="low_iid", high_iid="high_iid", low_MA1="low_MA1", high_MA1="high_MA1")
levels(df_est3$noise_setting_name) <- c(expression("No~noise"),
                                        "Low~i.i.d.~noise",
                                        "High~i.i.d.~noise",
                                        "Low~MA(1)~noise",
                                        "High~MA(1)~noise")


# Recode factor "type_estimator"
df_est3$type_estimator <- recode_factor(df_est3$type_estimator,
                                       RV="RV",
                                       RV_AC1="RV~AC(1)")


# 1. Plot Bias and distinguish between estimation of sampling (columns) and noise (rows)
ggplot(df_est3 %>% 
         dplyr::filter(type_estimator=="RV", MAcoeff_noise==0, M>=13)) +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=26), lty="dashed", col="grey40") +
  geom_vline(aes(xintercept=390), lty="dashed", col="grey40") +
  geom_line(aes(x=M, y=bias_rel, col=sampling_type)) +
  facet_grid(noise_setting_name~sampling_est, scales="free", labeller=label_parsed) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=c(13,26,78,390,2340),
                     trans='log2') +
  scale_colour_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  ylab("Relative bias") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1) +
  guides(color=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots/Sim_Bias_RV_iidnoise.pdf", width=8, height=6.5)



# 2. Plot MSE and distinguish between estimation of sampling (culomns) and noise (rows)
ggplot(df_est3 %>% 
         dplyr::filter(type_estimator=="RV", MAcoeff_noise==0, M>=13)) +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=26), lty="dashed", col="grey40") +
  geom_vline(aes(xintercept=390), lty="dashed", col="grey40") +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_type)) +
  facet_grid(noise_setting_name~sampling_est, scales="fixed", labeller=label_parsed) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(13,26,78,390,2340),
                     trans='log2') +
  scale_colour_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  ylab("Relative RMSE") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1) +
  guides(color=guide_legend(title="Sampling scheme"))

ggsave("simulations/plots/Sim_RMSE_rel_RV_iidnoise.pdf", width=8, height=6.5)




######## PLOTS UNDER NOISE

# 3. BIAS UNDER NOISE PLOT
p <- ggplot(df_est3 %>% 
              dplyr::filter(M>=13, 
                            noise_setting_name!="No~noise",
                            sampling_est %in% c('(C)~~Daily~sampling', expression((B)~~Past~average~~~Delta~"="~250)))) +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=26), lty="dashed", col="grey40") +
  geom_vline(aes(xintercept=390), lty="dashed", col="grey40") +
  geom_line(aes(x=M, y=bias_rel, col=sampling_type)) +
  #facet_grid(type_estimator~noise_setting_name, scales="free", labeller=label_parsed) +
  facet_grid(noise_setting_name~sampling_est+type_estimator, scales="free", labeller=label_parsed) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=c(13,26,78,390,2340),
                     trans='log2') +
  scale_colour_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  ylab("Relative bias") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1) +
  guides(color=guide_legend(title="Sampling scheme"))

# Manually insert irregular spaces between facets
p_new = ggplot_gtable(ggplot_build(p))
p_new$widths[8] = 3*p_new$widths[8]
p_new$heights[11] = 3*p_new$heights[11]
p_new3 <- arrangeGrob(p_new) 

ggsave("simulations/plots/Sim_Bias_rel_RVAC1.pdf", plot=p_new3, width=8, height=8.5)



# 4. MSE UNDER NOISE PLOT
p <- ggplot(df_est3 %>% 
              dplyr::filter(M>=13, 
                            noise_setting_name!="No~noise",
                            sampling_est %in% c('(C)~~Daily~sampling', expression((B)~~Past~average~~~Delta~"="~250)))) +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=26), lty="dashed", col="grey40") +
  geom_vline(aes(xintercept=390), lty="dashed", col="grey40") +
  geom_line(aes(x=M, y=RMSE_rel, col=sampling_type)) +
  facet_grid(noise_setting_name~sampling_est+type_estimator, scales="fixed", labeller=label_parsed) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(breaks=c(13,26,78,390,2340),
                     trans='log2') +
  scale_colour_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  ylab("Relative RMSE") +
  theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1) +
  guides(color=guide_legend(title="Sampling scheme"))


# Manually insert irregular spaces between facets
p_new = ggplot_gtable(ggplot_build(p))
p_new$widths[8] = 3*p_new$widths[8]
p_new$heights[11] = 3*p_new$heights[11]
p_new3 <- arrangeGrob(p_new) 

ggsave("simulations/plots/Sim_RMSE_rel_RVAC1.pdf", plot=p_new3, width=8, height=8.5)






