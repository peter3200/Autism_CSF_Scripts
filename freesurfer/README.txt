#This is the README file for scripts used to employ FreeSurfer (used in an intial test-retest analysis of ventricular volume with 28 HCP scans).

#Contents
#1. freesurfer_batch.sh This is an all-purpose wrapper script used to submit FreeSurfer jobs using sbatch to a SLURM system. 
#2. freesurfer_job.sh This is a job script used to submit recon-all jobs to a SLURM system. Use in conjunction with the batch script. This is the first step in the longitudinal pipeline.
#3. freesurfer_job_base.sh This is a job script used to submit the "base" jobs to a SLURM system. Use in conjunction with the batch script. This is the second step in the longitudinal pipeline.
#4. freesurfer_job_long.sh This is a job script used to submit the "longitudinal" jobs to a SLURM system. Use in conjunction with the batch script. This is the third step in the longitudinal pipeline. *Note: this takes considerably shorter amount of time to run than the first two steps.
#5. freesurfer_stats_table.sh This script uses aseg2table to grab the volume stats from the longitudinal job results and combine them into a usable table (.txt format, which can be imported into excel or some other program). No need for the batch script with this script! 

#For instructions on running FreeSurfer's longitudinal pipeline, please see their documentation: https://surfer.nmr.mgh.harvard.edu/fswiki/LongitudinalProcessing 


# Note: Freesurfer values for total brain volume and cortical thickness (as described in Peterson et al.) were derived using the cross-sectional Freesurfer pipeline. See the cross_sectional folder for those scripts.
