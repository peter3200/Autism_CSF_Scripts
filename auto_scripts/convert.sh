#!/bin/bash

subjDir=/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/output_1yr

time=time5

for i in `cat /fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/code/subjids/${time}_t1_ids.txt`; do
	c3d ${subjDir}/${time}_t1/${i}/FinalMasking/${i}_T1_stx_stripped_EMS_withoutVent_MID02.nrrd -o ${subjDir}/${time}_t1/${i}/${i}_MID02.nii.gz
done

