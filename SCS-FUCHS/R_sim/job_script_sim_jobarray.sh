#!/bin/bash
#SBATCH --partition=fuchs
#SBATCH --nodes=1
#SBATCH --ntasks=20
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=20
#SBATCH --mem-per-cpu=0
#SBATCH --time=200:00:00
#SBATCH --array=0-399:20
#SBATCH --mail-type=END,FAIL
#SBATCH --output=RVTTSV/data/simulation/logs/job_%A_%a.out
#SBATCH --error=RVTTSV/data/simulation/logs/job_%A_%a.err
 
my_task() {
     # Print the given "global task number" with leading zeroes
     # followed by the hostname of the executing node.
     K=$(printf "%03d" $1)
     echo "$K: $HOSTNAME"
 
    Rscript /home/fuchs/agmisc/dimitriadis/RVTTSV/R_sim/sim_BashLoop_FUCHS_RR2.R $1  # Pass J to R script
}
 
#
# Every 20-task block will run on a separate node.
 
for I in $(seq 20); do
     # This is the "global task number". Since we have an array of
     # XYZ tasks, J will range from 1 to XYZ.
     J=$(($SLURM_ARRAY_TASK_ID+$I))
 
     # Put each task into background, so that tasks are executed
     # concurrently.
     my_task $J &
 
     # Wait a little before starting the next one.
     sleep 1
done
 
# Wait for all child processes to terminate.
wait
