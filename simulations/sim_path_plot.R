library(tidyverse)
library(lubridate)
library(padr)
library(slider)
library(doParallel)

source("sample_schemes_est.R")
source("simulations/sim_TTSV.R")
source("lambda_est.R")
source("varsigma_est.R")


TT <- 23400

# Deterministic trends: estimated and averaged IBM data
IBM_intensities_avg <- readRDS(file = "simulations/data/IBM_intensities_est.rds")

LamSig_det <-  IBM_intensities_avg %>%
  dplyr::filter(tau!=23400) %>%
  summarize(time = 0:TT,
            lambda = approx(x=tau, y=lambda_avg, xout=0:TT, rule=2)$y,
            varsigma = approx(x=tau, y=varsigma_avg, xout=0:TT, rule=2)$y)



################################################################################
################################################################################
###     PLOTTING

### Plot true lambda and varsigma curves for a simulated test set
set.seed(1)
dat_TTSV_test <- sim_TTSV(days=5,
                          lambda_det=LamSig_det$lambda, 
                          varsigma_det=LamSig_det$varsigma, 
                          TT=TT+1)

# Plotting df for the simulated days
df_plot_sim <- dat_TTSV_test$prices %>% 
  filter(Date<=3) %>%
  mutate(varsigma2 = varsigma^2,
         sigma2 = lambda*varsigma2) %>%
  select(Date, SecSinceStart, Price, lambda, varsigma2, sigma2) %>%
  pivot_longer(cols=c("Price", "lambda", "varsigma2", "sigma2"), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(Day=Date,
         Time=hms::as_hms(SecSinceStart + 34200),
         measure = factor(measure, levels=c("Price", "sigma2", "lambda", "varsigma2"))) %>%
  select(Time, Day, measure, value)


# Plotting df for the deterministic intensities
df_plot_intensities <- LamSig_det %>% 
  mutate(varsigma2 = varsigma^2,
         sigma2 = lambda*varsigma2) %>%
  select(-varsigma) %>%
  pivot_longer(cols=c("lambda", "varsigma2", "sigma2"), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(Time = hms::as_hms(time + 34200),
         measure = factor(measure, levels=c("sigma2", "lambda", "varsigma2"))) %>%
  mutate(Day=10000) %>%
  select(Time, Day, measure, value)

# Join df's
df_plot <- bind_rows(df_plot_sim, df_plot_intensities) %>%
  mutate(Day=factor(Day))

# Set factor levels to obtain nice facet captions
levels(df_plot$measure) <- c("Simulated~price",
                             expression(Simulated~spot~variance~sigma^2*(t)),
                             expression(Simulated~trading~intensity~lambda(t)),
                             expression(Simulated~tick~variance~varsigma^2*(t)))

# Plot the first few days of price/lambda/varsigma days
ggplot(df_plot) +
  geom_line(aes(x=Time, y=value, col=Day)) +
  facet_wrap(~measure, ncol=2, scales="free", labeller=label_parsed) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("#D39200","#00C19F", "#DB72FB", "black")) +
  scale_x_time(breaks = scales::date_breaks("1 hours"),
               labels = scales::time_format(format="%H:%M"))


ggsave("simulations/plots_RR/Sim_Price_Path.pdf", width=8, height=6)
