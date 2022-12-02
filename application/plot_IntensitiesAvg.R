library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(slider)
library(scales)

source("sample_schemes_est.R")
source("lambda_est.R")
source("varsigma_est.R")


dir_base <- getwd()
asset <- "IBM"

# Load raw asset ticks
dat <- readRDS(file = paste0(dir_base,"/application/data/filtered_rds/",asset,"_ticks.rds"))


grid <- seq(0, secs_trading, by=30)
secs_trading <- 23400
h <- 2000
m_factor <- 3000 
H <- 40
  
# Estimate daily intensity
df_intensity_est <- dat %>% 
  dplyr::filter(Date >= as_date("2018-01-01"),
                Date <= as_date("2018-12-31")) %>%
  group_by(Date) %>%
  summarize(tau = grid,
            lambda_hat = lambda_est(tau_grid=grid, 
                                    t_tick=SecSinceStart,
                                    TT=secs_trading, 
                                    k=k_Epanechnikov, 
                                    h=h),
            varsigma2_hat = varsigma2_est(tau_grid = grid,
                                          t_tick = SecSinceStart, 
                                          prices_tick = LogPrice, 
                                          TT=secs_trading, 
                                          k=k_Epanechnikov, 
                                          H=H,
                                          m_factor=m_factor),
            sigma2_hat = lambda_hat*varsigma2_hat) %>%
  rename(SecSinceStart=tau)


df_intensity_avg <- df_intensity_est %>%
  dplyr::filter(Date >= as_date("2018-01-01"),
                Date <= as_date("2018-12-31"),
                SecSinceStart != 23400) %>%
  group_by(SecSinceStart) %>%
  summarize(lambda_avg = mean(lambda_hat),
            varsigma_avg = mean(sqrt(varsigma2_hat)),
            varsigma2_avg = mean(varsigma2_hat),
            sigma2_avg = mean(lambda_hat*varsigma2_hat)
) %>%
  ungroup() %>%
  summarize(Time=as_date("2015-01-01") + seconds(34200+SecSinceStart),
            lambda_rel=lambda_avg/max(lambda_avg),
            varsigma2_rel=varsigma2_avg/max(varsigma2_avg),
            sigma2_rel=sigma2_avg/max(sigma2_avg)) %>%
  pivot_longer(cols=!Time,
               names_to = "measure", 
               values_to = "value") 

df_intensity_avg$measure <- factor(df_intensity_avg$measure, levels=c("lambda_rel", "varsigma2_rel", "sigma2_rel"))


ggplot(df_intensity_avg) +
  geom_line(aes(x=Time, y=value, col=measure)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ylim(c(0,1)) +
  ylab("Relative value") +
  scale_colour_manual(name = "Measure  ", 
                      breaks = c("lambda_rel", "varsigma2_rel", "sigma2_rel"),
                      values = c("orange", "purple", "black"),
                      labels = expression(Tick~intensity~lambda(t), 
                                          Tick~variance~varsigma^2*(t), 
                                          Spot~variance~sigma^2*(t))) +
  scale_x_time(breaks = scales::date_breaks("1 hours"),
               labels = scales::time_format(format="%H:%M"))

ggsave("application/plots/plot_IntensityMeasures.pdf", width=8, height=4)




