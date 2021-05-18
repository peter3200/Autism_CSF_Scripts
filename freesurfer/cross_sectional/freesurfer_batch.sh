#!/bin/bash
time=time5

#for subj in `cat /fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/code/subjids/${time}_t1_ids.txt`; do
for subj in 28616; do
	sbatch \
	-o ~/logfiles/${1}/output_${subj}.txt \
	-e ~/logfiles/${1}/error_${subj}.txt \
	~/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/code/freesurfer_code/freesurfer_job.sh \
	${subj}
	sleep 1
done
