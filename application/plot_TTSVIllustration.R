library(tidyverse)
library(lubridate)
library(ggh4x)

dat <- readRDS(file = "application/data/filtered_rds/IBM_ticks.rds")

dat_day <- dat %>% dplyr::filter(Date == "2015-05-01")

l <- 4
df_sampling <- tibble(Time = as_datetime("2015-05-01 09:30:00") + seconds(seq(0,60,length.out=l)),
                      type = "sampling",
                      number = 1:l,
                      axis_label = factor(paste0("tau[",number-1,"]")))
                      
df_plot <- dat_day %>%
  dplyr::filter(DateTime <= as_datetime("2015-05-01 09:31:30")) %>%
  slice(c(4,5,7,8,11,15,16,18,20,21,22)) %>%
  mutate(Time=DateTime,
         NextTick = lead(DateTime),
         type="price",
         number=row_number()) %>%
  slice(-11) %>%
  mutate(axis_label=factor(paste0("t[",number,"]")),
         axis_label=factor(axis_label, levels=paste0("t[",number,"]"))) %>%
  select(Time, NextTick, LogPrice, type, number, axis_label) %>%
  bind_rows(df_sampling) %>%
  mutate(axis_col=ifelse(type == "price", "blue", "red")) %>%
  mutate(LogPriceHlp = LogPrice) %>%
  arrange(Time) %>%
  tidyr::fill(c("LogPriceHlp"), .direction="down") %>%
  arrange(type, Time)

# Set tick breaks
tick_breaks <- df_plot$Time

# Joint plot of N and P
df_plot_joint <- bind_rows(df_plot %>% rename(value = LogPrice) %>% mutate(type_facet="P(t)"),
                           df_plot %>% mutate(value = ifelse(type=="price", number, NA),
                                              type_facet = "N(t)",
                                              LogPriceHlp = NA))

ggplot(df_plot_joint %>% dplyr::filter(type!="sampling")) + 
  geom_vline(data=df_plot_joint %>% dplyr::filter(type=="sampling"), aes(xintercept=Time), col="red", lty="dashed") +
  geom_point(data=df_plot_joint %>% dplyr::filter(type=="sampling"), aes(x=Time, y=LogPriceHlp), col="red", shape=15, size=4) +
  geom_segment(aes(x=Time, xend=NextTick, y=value, yend=value), col="grey40") +
  geom_point(aes(x=Time, y=value), col="blue") +
  facet_wrap(~type_facet, ncol=1, scales="free") +
  #             labeller = as_labeller(c(P(t) = "Log-price", N(t) = experession(t[i])))) +
  ggh4x::force_panelsizes(rows = c(0.5, 1)) +
  scale_x_continuous(breaks = tick_breaks,
                     labels = parse(text = levels(df_plot$axis_label)),
                     minor_breaks=c(1)) +
  theme_bw() +
  ylab("Value") +
  theme(axis.text.x = element_text(colour = df_plot$axis_col),
        axis.ticks.x = element_line(colour = df_plot$axis_col)) +
  coord_cartesian(xlim =c(NA, as_datetime("2015-05-01 09:31:10.000")))

ggsave("application/plots/plot_TTSVModelIllustration_Joint.pdf", width=8, height=5)





# # Plot of P(t) only
# ggplot(df_plot) + 
#   geom_vline(data=df_sampling, aes(xintercept=Time), col="red", lty="dashed") +
#   geom_segment(aes(x=Time, xend=NextTick, y=LogPrice, yend=LogPrice), col="grey40") +
#   geom_point(aes(x=Time, y=LogPrice), col="blue") +
#   geom_point(data=df_plot%>%dplyr::filter(type=="sampling"), aes(x=Time, y=LogPriceHlp), col="red", shape=1, size=5) +
#   scale_x_continuous(breaks = tick_breaks,
#                      labels = parse(text = c(NA,levels(df_plot$axis_label)[-1])),
#                      minor_breaks=c(1)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(colour = df_plot$axis_col)) 
# 
# ggsave("application/plots/plot_TTSVModelIllustration.pdf", width=8, height=4)
# 
# 
# # Bad plot of N(t) only
# ggplot(df_plot %>% dplyr::filter(type=="price")) +
#   geom_step(aes(x=Time, y=number)) +
#   theme_bw()
# 
# ggsave("application/plots/plot_TTSVModelIllustration_N.pdf", width=8, height=4)




