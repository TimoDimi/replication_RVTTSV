#! /bin/bash


for asset in AA AXP BA BAC CAT DIS GE GS HD HON HPQ IBM IP JNJ JPM KO MCD MMM MO MRK NKE PFE PG UTX VZ WMT XOM
do
    Rscript application/resample_stocks_RR.R $asset
done




for asset in AA AXP BA BAC CAT DIS GE GS HD HON HPQ IBM IP JNJ JPM KO MCD MMM MO MRK NKE PFE PG UTX VZ WMT XOM
do
for junk in 1 2
do
    Rscript application/application_RVest_RR.R $asset $junk
done
done