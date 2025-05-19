#!/bin/bash

# Copy data folder (with -r!)
scp -r /Users/timodimitriadis/Documents/Code/replication_RVTTSV/SCS-FUCHS/R_sim dimitriadis@fuchs.hhlr-gu.de:/home/fuchs/agmisc/dimitriadis/RVTTSV/


# Submit jobs remotely using SSH
ssh dimitriadis@fuchs.hhlr-gu.de << 'EOF'
    sbatch /home/fuchs/agmisc/dimitriadis/RVTTSV/R_sim/job_script_sim_jobarray.sh
EOF
