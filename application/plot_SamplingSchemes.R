library(tidyverse)
library(lubridate)

dir_base <- getwd()
asset <- "IBM"
M <- 26
date_plot <- as_date("2015-05-01")


# Load data
dat_resampled <- readRDS(file = paste0(dir_base,"/application/data/resampled_RR/",asset,"_prices_resampled_starting2012.rds"))
dat_ticks <- readRDS(file = paste0(dir_base,"/application/data/filtered_rds/",asset,"_ticks.rds"))

dat_ticks <- dat_ticks %>% 
  dplyr::filter(Date==date_plot) %>%
  rename(Time = DateTime)

dat_resampled <- dat_resampled %>% 
  dplyr::filter(Date==date_plot)

# Careful: This "filtering" only works for prices, not for returns!
dat_resampled_plot <- dat_resampled %>%
  dplyr::filter(sampling %in% c("CTS", "BTS_realized_rolling_avg50", "TTS_realized"),
                time_sampling %in% seq(from = as_datetime("2015-05-01 00:00:00"),
                                       to = as_datetime("2015-05-01 06:30:00"),
                                       by = 15*60)) %>%
  mutate(time_last_tick = time_last_tick + seconds(34200),
         sampling=factor(sampling, levels=c('CTS','TTS_realized','BTS_realized_rolling_avg50'))) %>%
  rename(Time = time_last_tick)

# Change names in plot layout
levels(dat_resampled_plot$sampling) <- c("CTS     ",
                                         "rTTS     ",
                                         "rBTS     ")
legend_title <- "Sampling Scheme     "


# Actual Plot
ggplot() +
  facet_wrap(~sampling, ncol=1) +
  geom_point(data=dat_resampled_plot,
             aes(x=Time, y=Price, col=sampling, shape=sampling), size=3) +
  geom_vline(data=dat_resampled_plot,
             aes(xintercept=Time, col=sampling)) + 
  geom_line(data=dat_ticks, aes(x=Time, y=Price)) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  scale_x_time(breaks = scales::date_breaks("1 hours"),
               labels= scales::time_format(format="%H:%M")) +
  guides(color=guide_legend(title=legend_title),
         shape=guide_legend(title=legend_title))

ggsave("application/plots/plot_SamplingSchemes.pdf", width=8, height=6)

