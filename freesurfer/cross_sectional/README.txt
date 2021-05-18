#Scripts used to run the cross-sectional Freesurfer recon-all pipeline and obtain TBV and mean CT values.

#Contents
#1. freesurfer_batch.sh This is an all-purpose wrapper script used to submit FreeSurfer jobs using sbatch to a SLURM system. 
#2. freesurfer_job.sh This is a job script used to submit recon-all jobs to a SLURM system. Use in conjunction with the batch script. 
#3. freesurfer_stats_table2.sh This script uses aseg2table to grab the volume stats from the job results and combine them into a usable table (.csv format, which can be imported into excel or some other program).
#4. freesurfer_stats_table3.sh This script uses aparc2table to grab the parcellation stats from the job results and combine them into a usable table (.csv format, which can be imported into excel or some other program).

#Documentation for the Freesurfer recon-all pipeline can be found here: https://surfer.nmr.mgh.harvard.edu/fswiki/recon-all
