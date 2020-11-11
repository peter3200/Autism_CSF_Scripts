#README file for Auto_EACSF execution scripts (and grabbing stats)

#Purpose: Extract extra-axial CSF and compile data in a csv file

#Script Descriptions
#1. 7Sept_param.json This is the json file holding the parameters for running Auto_EACSF. Specifications regarding file locations, atlases, and other parameters can be found here.
#2. auto_job_T1only.sh This shell script is a SLURM "job" script designed to run Auto_EACSF on a single subject's image. This script was originally written by Cory Coleman at UC Davis and modified by M Peterson
#3. auto_wrap_T1only.sh This shell script is a wrapper script for #2. Will use sbatch to submit a job for each subject in a subjids text file list.
#4. convert.sh This shell script will convert MID02.nrrd files (Auto_EACSF output) to MID02.nii.gz files (since fsleyes refuses .nrrd files). 
#5. fsleyes_launch.sh This shell script launches FSLeyes for each T1w and MID02 image pair using a text list of subjids. Used to QC images.
#6. GenerateStats.sh This shell script calls CMTK (https://www.nitrc.org/projects/cmtk) to grab the number of CSF voxels in each MID02 file for each participant. This number is placed in a text file for each participant.
#7. GetAllFinalResults.py This python script concatenates the individual stats text files generated in #6 into a .csv file. Written by Cory Coleman, UC Davis.



