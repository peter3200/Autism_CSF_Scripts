#!/bin/bash

#SBATCH --time=15:00:00   # walltime
#SBATCH --ntasks=1   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=12540M   # memory per CPU core

# Compatibility variables for PBS. Delete if not needed.
export PBS_NODEFILE=`/fslapps/fslutils/generate_pbs_nodefile`
export PBS_JOBID=$SLURM_JOB_ID
export PBS_O_WORKDIR="$SLURM_SUBMIT_DIR"
export PBS_QUEUE=batch

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE

# LOAD MODULES, INSERT CODE, AND RUN YOUR PROGRAMS HERE
subj=$1
time=time5
subjDir=/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/Auto_EACSF_ready/${time}/${subj}

T1_Dir=~/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/Auto_EACSF_ready/${time}
T1_file_extension='.nii.gz'                   

BinDir='/fslhome/mpeter55/Downloads/Software/Auto_EACSF-1.7.7-Linux/bin/'
ParamsTemplate=/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/code/code/7Sept_param.json
OutputDir=/fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/output_1yr/${time}

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$BinDir/../lib"
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/fslhome/mpeter55/lib64/ld-linux-x86-64.so.2
export QT_PLUGIN_PATH=/fslhome/mpeter55/Downloads/Software/Auto_EACSF_v1.4.1_RHL6/plugins:$QT_PLUGIN_PATH
export PATH=/fslhome/mpeter55/Downloads/Software/Auto_EACSF_v1.4.1_RHL6/bin:$PATH
export LD_LIBRARY_PATH=/fslhome/mpeter55/usr/lib/libhistory.so.5:$LD_LIBRARY_PATH

T1_filename=$T1_Dir/${subj}/anat/${subj}_T1$T1_file_extension

SubjectsOutputDir="$OutputDir/${subj}"
mkdir $SubjectsOutputDir

T1_Unzipped=$SubjectsOutputDir/${subj}_T1.nii
echo "gunzip -c $T1_filename > $T1_Unzipped"
gunzip -c $T1_filename > $T1_Unzipped

cp $ParamsTemplate $SubjectsOutputDir/template001.json
sed "s@T1FILE@$T1_Unzipped@g" $SubjectsOutputDir/template001.json > $SubjectsOutputDir/template002.json
sed "s@OUTDIR@$SubjectsOutputDir@g" $SubjectsOutputDir/template002.json > $SubjectsOutputDir/configFile.json

cd $BinDir
echo "RUNNNING: ./Auto_EACSF -p $SubjectsOutputDir/configFile.json --noGUI "
./Auto_EACSF --param $SubjectsOutputDir/configFile.json --noGUI




