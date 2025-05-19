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
IBM_intensities_avg <- readRDS(file = "simulations/IBM_intensities_est.rds")

LamSig_det_raw <-  IBM_intensities_avg %>%
  dplyr::filter(tau!=23400) %>%
  reframe(time = 0:TT,
          lambda = approx(x=tau, y=lambda_avg, xout=0:TT, rule=2)$y,
          varsigma = approx(x=tau, y=varsigma_avg, xout=0:TT, rule=2)$y)


# How many ticks should there be per day?
lambda_setting <- 8000

# Control deterministic component for the Indepdent Process
Expected_ticks_raw_independent <- sum(LamSig_det_raw$lambda)
LamSig_det_independent <- LamSig_det_raw %>% mutate(lambda = lambda * lambda_setting/Expected_ticks_raw_independent,
                                                    varsigma = varsigma / sqrt(lambda_setting/Expected_ticks_raw_independent) )


# Hawkkes-type settings
a_pos_lambda_factor <- 0.05
a_neg_lambda_factor <- 0.1
b_lambda_factor <- 0.25
eta <- mean(c(a_pos_lambda_factor, a_neg_lambda_factor)) / b_lambda_factor

c_Hawkes_varsigma <- case_when(
  lambda_setting == 2000 ~ 1.045,
  lambda_setting == 8000 ~ 1,
  lambda_setting == 32000 ~ 0.785,
)

# Weigh lambda by how many ticks we want to have (approximately) per day in the end
Expected_ticks_raw_Hawkes <- 1/(1-eta) * sum(LamSig_det_raw$lambda)
LamSig_det_Hawkes <- LamSig_det_raw %>% mutate(lambda = lambda * lambda_setting/Expected_ticks_raw_Hawkes,
                                               varsigma = varsigma * sqrt(c_Hawkes_varsigma) / sqrt(lambda_setting/Expected_ticks_raw_Hawkes/(1-eta)) )



################################################################################
################################################################################
###     PLOTTING

### Plot true lambda and varsigma curves for a simulated test set
set.seed(3)

# Indpendent TTSV process simulation
dat_TTSV_sim <- sim_TTSV(days=3,
                         lambda_det=LamSig_det_independent$lambda,  
                         varsigma_det=LamSig_det_independent$varsigma, 
                         TT=TT+1)

# Hawkes-type TTSV process simulation
lambda_mean_sim <- mean(LamSig_det_Hawkes$lambda)
varsigma_mean_sim <- mean(LamSig_det_Hawkes$varsigma)

dat_TTSV_Hawkes_sim <- simulate_TTSV_Hawkes(days=3,
                                            TT = TT+1,
                                            lambda_det = LamSig_det_Hawkes$lambda, 
                                            varsigma_det = LamSig_det_Hawkes$varsigma, # Account for the Hawkes type effect in varsigma
                                            a_pos_lambda = a_pos_lambda_factor*lambda_mean_sim,
                                            a_neg_lambda = a_neg_lambda_factor*lambda_mean_sim, 
                                            b_lambda = b_lambda_factor*lambda_mean_sim,
                                            a_pos_varsigma = 0*varsigma_mean_sim,
                                            a_neg_varsigma = 0.1*varsigma_mean_sim,
                                            b_varsigma = 0.5,
                                            rho_intensities=0.3)


##### Temporary checks!!!
dim(dat_TTSV_Hawkes_sim)[1]/3
1/(1-eta) * sum(LamSig_det_Hawkes$lambda)
sum(dat_TTSV_Hawkes_sim$lambda)  

dim(dat_TTSV_sim$prices)[1]/3
sum(LamSig_det_independent$lambda)

# Approximate Independent IV
full_join(dat_TTSV_sim$intensities %>%
            group_by(Date) %>%
            summarise(IV = sum(lambda*varsigma^2)),
          dat_TTSV_sim$prices %>%
            group_by(Date) %>%
            summarise(rIV = sum(varsigma^2)),
          by="Date")

# Approximate Hawkes IV
full_join(dat_TTSV_Hawkes_sim %>%
            group_by(Date) %>%
            reframe(SecSinceStart_linear = 0:23400,
                    lambda_approx = approx(x=SecSinceStart, y=lambda, xout=SecSinceStart_linear, rule=2)$y,
                    varsigma_approx = approx(x=SecSinceStart, y=varsigma, xout=SecSinceStart_linear, rule=2)$y) %>%
            rename(SecSinceStart=SecSinceStart_linear, lambda=lambda_approx, varsigma=varsigma_approx) %>%
            group_by(Date) %>%
            summarise(IV = sum(lambda*varsigma^2)),
          dat_TTSV_Hawkes_sim %>%
            group_by(Date) %>%
            summarise(rIV = sum(varsigma^2)),
          by="Date")



# Joint plot for both Processes

# Obtain price and intensity DFs
df_prices <- bind_rows(dat_TTSV_sim$prices %>% mutate(Process="Independent"),
                       dat_TTSV_Hawkes_sim %>% mutate(Process="Hawkes"))

df_intensities <- bind_rows(
  dat_TTSV_sim$intensities %>% 
    mutate(Process="Independent"),
  dat_TTSV_Hawkes_sim %>%
    group_by(Date) %>%
    reframe(SecSinceStart_linear = 0:23400,
            lambda_approx = approx(x=SecSinceStart, y=lambda, xout=SecSinceStart_linear, rule=2)$y,
            varsigma_approx = approx(x=SecSinceStart, y=varsigma, xout=SecSinceStart_linear, rule=2)$y) %>%
    rename(SecSinceStart=SecSinceStart_linear, lambda=lambda_approx, varsigma=varsigma_approx) %>% 
    mutate(Process="Hawkes")
)




# Plotting df for the simulated days
df_plot_sim <- df_prices %>% 
  filter(Date<=3) %>%
  mutate(varsigma2 = varsigma^2,
         sigma2 = lambda*varsigma2) %>%
  select(Date, SecSinceStart, Price, lambda, varsigma2, sigma2, Process) %>%
  pivot_longer(cols=c("Price", "lambda", "varsigma2", "sigma2"), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(Day=Date,
         Time=hms::as_hms(SecSinceStart + 34200),
         measure = factor(measure, levels=c("Price", "sigma2", "lambda", "varsigma2"))) %>%
  select(Time, Day, measure, value, Process)



# Plotting df for the deterministic intensities
df_plot_intensities <- 
  bind_rows(LamSig_det_independent %>% 
              mutate(Process="Independent",
                     varsigma2 = varsigma^2,
                     sigma2 = lambda*varsigma2),
            LamSig_det_raw %>% 
              mutate(Process="Hawkes",
                     varsigma2 = varsigma^2,
                     sigma2 = lambda*varsigma2)) %>%
  select(-varsigma) %>%
  pivot_longer(cols=c("lambda", "varsigma2", "sigma2"), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(Time = hms::as_hms(time + 34200),
         measure = factor(measure, levels=c("sigma2", "lambda", "varsigma2"))) %>%
  mutate(Day=10000) %>%
  select(Time, Day, measure, value, Process)

# Join df's
df_plot <- bind_rows(df_plot_sim, df_plot_intensities) %>%
  mutate(Day=factor(Day))

# Recode factor
df_plot$Process <- recode_factor(df_plot$Process,
                          Independent="Independent", Hawkes="Hawkes")
levels(df_plot$Process) <- c("Independent~TTSV~process",
                             "Hawkes-type~TTSV~process")


# Set factor levels to obtain nice facet captions
levels(df_plot$measure) <- c(expression(Simulated~price~P*(t)),
                             expression(Simulated~spot~variance~sigma^2*(t)),
                             expression(Simulated~trading~intensity~lambda(t)),
                             expression(Simulated~tick~variance~varsigma^2*(t)))


# Plot the first few days of price/lambda/varsigma days
ggplot(df_plot) +
  geom_line(aes(x=Time, y=value, col=Day)) +
  ggh4x::facet_grid2(Process~measure, scales = "free", independent = "all", labeller=label_parsed) +
  # facet_grid(Process~measure, scales="free", labeller=label_parsed) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("#D39200","#00C19F", "#DB72FB", "black")) +
  scale_x_time(breaks = scales::date_breaks("2 hours"),
               labels = scales::time_format(format="%H:%M"))


ggsave("simulations/plots_RR2/Sim_Price_Paths_RR2.pdf", width=10, height=5) 








###############################################################################################
###################    OLD: Only one plot
###############################################################################################

# # Obtain price and intensity DFs
# df_prices <- dat_TTSV_Hawkes_sim
# 
# df_intensities <- dat_TTSV_Hawkes_sim %>%
#   group_by(Date) %>%
#   reframe(SecSinceStart_linear = 0:23400,
#           lambda_approx = approx(x=SecSinceStart, y=lambda, xout=SecSinceStart_linear, rule=2)$y,
#           varsigma_approx = approx(x=SecSinceStart, y=varsigma, xout=SecSinceStart_linear, rule=2)$y) %>%
#   rename(SecSinceStart=SecSinceStart_linear, lambda=lambda_approx, varsigma=varsigma_approx)
# 
# 
# 
# # Plotting df for the simulated days
# df_plot_sim <- df_prices %>% 
#   filter(Date<=3) %>%
#   mutate(varsigma2 = varsigma^2,
#          sigma2 = lambda*varsigma2) %>%
#   select(Date, SecSinceStart, Price, lambda, varsigma2, sigma2) %>%
#   pivot_longer(cols=c("Price", "lambda", "varsigma2", "sigma2"), 
#                names_to = "measure", 
#                values_to = "value") %>%
#   mutate(Day=Date,
#          Time=hms::as_hms(SecSinceStart + 34200),
#          measure = factor(measure, levels=c("Price", "sigma2", "lambda", "varsigma2"))) %>%
#   select(Time, Day, measure, value)
# 
# 
# # Plotting df for the deterministic intensities
# df_plot_intensities <- LamSig_det %>% 
#   mutate(lambda =  1/(1-eta)*lambda,
#          varsigma2 = varsigma^2,
#          sigma2 = lambda*varsigma2) %>%
#   select(-varsigma) %>%
#   pivot_longer(cols=c("lambda", "varsigma2", "sigma2"), 
#                names_to = "measure", 
#                values_to = "value") %>%
#   mutate(Time = hms::as_hms(time + 34200),
#          measure = factor(measure, levels=c("sigma2", "lambda", "varsigma2"))) %>%
#   mutate(Day=10000) %>%
#   select(Time, Day, measure, value)
# 
# # Join df's
# df_plot <- bind_rows(df_plot_sim, df_plot_intensities) %>%
#   mutate(Day=factor(Day))
# 
# # Set factor levels to obtain nice facet captions
# levels(df_plot$measure) <- c(expression(Simulated~price~P*(t)),
#                              expression(Simulated~spot~variance~sigma^2*(t)),
#                              expression(Simulated~trading~intensity~lambda(t)),
#                              expression(Simulated~tick~variance~varsigma^2*(t)))
# 
# # Plot the first few days of price/lambda/varsigma days
# ggplot(df_plot) +
#   geom_line(aes(x=Time, y=value, col=Day)) +
#   facet_wrap(~measure, ncol=2, scales="free", labeller=label_parsed) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   scale_colour_manual(values = c("#D39200","#00C19F", "#DB72FB", "black")) +
#   scale_x_time(breaks = scales::date_breaks("1 hours"),
#                labels = scales::time_format(format="%H:%M"))
# 
# 
# ggsave("simulations/plots_RR2/Sim_Price_Path_RR2.pdf", width=8, height=6)






