# replication_RVTTSV
Replication package for the paper "Efficient Estimation of Realized Variance in Time-Changed Diffusion Processes" (ADD LINK TO WORKING PAPER) 
================
by Timo Dimitriadis, Roxana Halbleib, Jeannine Polivka, and Sina Streicher

## Contents:

- Replication material for the simulations of the article in the folder 'simulations'. First run 'sim_main.R' (on a small cluster if possible) to run the simulation. The file 'sim_eval.R' produces the evaluating plots shown in the paper.
- Replication material for the application of the article in the folder 'application'. For the application, the necessary data files are not included in the repository as they cannot be made publicly available and require 28GB of storage space. Based on the data, the file 'prepare_ticks.R' prepars the data for processing and the file 'resample_stocks.R' applies the resampling techniques at a 10 second frequency, which are computationally very heavy and should be run on a cluster with sufficient memory. The file 'main_application.R' the computes the RV estimators based on different sampling schemes and frequencies, and the file 'application_eval.R' produces the plots given in the paper.

