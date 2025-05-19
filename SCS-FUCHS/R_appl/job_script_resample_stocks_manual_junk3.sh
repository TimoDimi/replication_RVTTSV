#!/bin/bash
#SBATCH --job-name=resample_stocks_junk3       # Job name
#SBATCH --partition=fuchs                # Partition (queue) to use
#SBATCH --nodes=1                        # Number of nodes (6 nodes)
#SBATCH --ntasks=1                      # Total number of tasks (27 tasks in total)
#SBATCH --ntasks-per-node=1              # Number of tasks per node (5 tasks per node)
#SBATCH --cpus-per-task=1                # CPUs per task (1 CPU per task)
#SBATCH --mem=0                          # Maximal memeory given the above setup!
#SBATCH --time=100:00:00                 # Maximum run time
#SBATCH --no-requeue                     # Do not requeue failed jobs
#SBATCH --mail-type=END,FAIL             # Notify when the job fails or is done
#SBATCH --output=RVTTSV/data/log_appl/job_resample_junk3.out
#SBATCH --error=RVTTSV/data/log_appl/job_resample_junk3.err


Rscript /home/fuchs/agmisc/dimitriadis/RVTTSV/R_appl/resample_stocks_FUCHS_RR2_junk3.R

# Optional: If you want a brief delay after the R script
sleep 1
