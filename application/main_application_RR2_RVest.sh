#! /bin/bash

for asset in AA AXP BA BAC CAT DIS GE GS HD HON HPQ IBM IP JNJ JPM KO MCD MMM MO MRK NKE PFE PG UTX VZ WMT XOM
do
    Rscript application/application_RVest_RR2.R $asset
done
