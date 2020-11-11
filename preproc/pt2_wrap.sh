#!/bin/bash

rootDir=/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data
codeDir=${rootDir}/code/preproc
textDir=${rootDir}/code/subjids
dataDir=${rootDir}/BIDS_data
outDir=${rootDir}/preprocessed
var=`date +"%Y%m%d-%H%M%S"`
time=time5


for subj in `cat ${textDir}/${time}_t1_ids.txt`; do
		mkdir -p /fslhome/mpeter55/logfiles/${var}/${time}/${subj}
		sbatch \
		-o /fslhome/mpeter55/logfiles/${var}/${time}/${subj}/${subj}_output_log.txt \
		-e /fslhome/mpeter55/logfiles/${var}/${time}/${subj}/${subj}_error_log.txt \
		${codeDir}/preproc_parallel_batch_pt2.sh ${subj}
		
		sleep 1
done

