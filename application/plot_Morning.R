library(tidyverse)
library(lubridate)
library(grid)

dat <- readRDS(file = "application/data/filtered_rds/IBM_ticks.rds")

df_plot <- bind_rows(
  dat %>%
    dplyr::filter(DateTime >= as_datetime("2015-05-01 9:45:00"), 
                  DateTime <= as_datetime("2015-05-01 9:48:00")) %>%
    mutate(Daytime = "Morning",
           NextTick = lead(DateTime)),
  dat %>%
    dplyr::filter(DateTime >= as_datetime("2015-05-01 15:57:00"), 
                  DateTime <= as_datetime("2015-05-01 16:00:00")) %>%
    mutate(Daytime = "Evening",
           NextTick = lead(DateTime))) %>%
  mutate(Time=DateTime,
         Daytime_f = factor(Daytime, levels=c('Morning','Evening')))


ggplot(df_plot) + 
  geom_segment(aes(x=Time, xend=NextTick, y=LogPrice, yend=LogPrice), col="grey40") +
  geom_point(aes(x=Time, y=LogPrice), col="blue") +
  facet_wrap(~Daytime_f, scales="free_x") +
  ylab("Log-price") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines"))

ggsave("application/plots/plot_MorningEvening.pdf", width=8, height=3)

