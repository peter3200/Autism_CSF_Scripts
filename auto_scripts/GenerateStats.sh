#!/bin/bash

GenDir=/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data
ResultsDir=${GenDir}/output_1yr
time=time5

for i in `cat ${GenDir}/code/subjids/${time}_t1_ids.txt`; do
	cmtk statistics ${ResultsDir}/${time}_t1/${i}/FinalMasking/${i}*_MID02.nrrd > ${ResultsDir}/${time}_t1/${i}/${i}_FinalStats.txt
	echo Calculating statistics for ${i}
done

