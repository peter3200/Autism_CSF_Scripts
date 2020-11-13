#!/bin/bash

for subj in `cat /fslhome/mpeter55/compute/HCP_trials/All_HCP_Data/code/1.7.7/MPR2_ids.txt`; do
	sbatch \
	-o ~/logfiles/FS/table_output_${subj}.txt \
	-e ~/logfiles/FS/table_error_${subj}.txt \
	~/compute/HCP_trials/All_HCP_Data/code/freesurfer_stats_table.sh \
	${subj}
	sleep 1
done
