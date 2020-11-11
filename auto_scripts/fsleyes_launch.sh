#!/bin/bash

genDir=/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data

time=time5


subjDir=${genDir}/output_1yr/${time}_t1

for i in `cat ${genDir}/code/subjids/${time}_t1_ids.txt`; do
fsleyes ${subjDir}/${i}/SkullStripping/${i}_T1_stx.nii.gz ${subjDir}/${i}/${i}_MID02.nii.gz  
done
