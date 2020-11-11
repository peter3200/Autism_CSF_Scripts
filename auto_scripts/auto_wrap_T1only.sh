#!/bin/bash

scriptDir=/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/code/code
var=`date +"%Y%m%d-%H%M%S"`
time=time5

for i in `cat /fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/code/subjids/${time}_t1_ids.txt`;do
	mkdir -p /fslhome/mpeter55/logfiles/${var}
	sbatch \
	-o /fslhome/mpeter55/logfiles/$var/${i}_output_log.txt \
	-e /fslhome/mpeter55/logfiles/$var/${i}_error_log.txt \
	${scriptDir}/auto_job_T1only.sh ${i}

	sleep 1
done
