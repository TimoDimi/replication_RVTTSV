#! /bin/bash

# Copy data folder (with -r!)
scp -r /Users/timodimitriadis/Documents/Code/replication_RVTTSV/SCS-FUCHS/R_appl dimitriadis@fuchs.hhlr-gu.de:/home/fuchs/agmisc/dimitriadis/RVTTSV/


# Submit jobs remotely using SSH
ssh dimitriadis@fuchs.hhlr-gu.de << 'EOF'
    sbatch /home/fuchs/agmisc/dimitriadis/RVTTSV/R_appl/job_script_resample_stocks_manual_junk1.sh
    sbatch /home/fuchs/agmisc/dimitriadis/RVTTSV/R_appl/job_script_resample_stocks_manual_junk2.sh
    sbatch /home/fuchs/agmisc/dimitriadis/RVTTSV/R_appl/job_script_resample_stocks_manual_junk3.sh
    sbatch /home/fuchs/agmisc/dimitriadis/RVTTSV/R_appl/job_script_resample_stocks_manual_junk4.sh
EOF
