#! /bin/bash

for i in {1..192}                    
do
   Rscript simulations/sim_BashLoop.R $i
done



