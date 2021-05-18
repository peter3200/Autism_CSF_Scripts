#!/bin/bash

#SBATCH --time=30:00:00   # walltime
#SBATCH --ntasks=1   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=16384M  # memory per CPU core
#SBATCH --mail-user=petemade@isu.edu
#SBATCH --mail-type=END

# Compatibility variables for PBS. Delete if not needed.
export PBS_NODEFILE=`/fslapps/fslutils/generate_pbs_nodefile`
export PBS_JOBID=$SLURM_JOB_ID
export PBS_O_WORKDIR="$SLURM_SUBMIT_DIR"
export PBS_QUEUE=batch

# Set the max number of threads to use for programs using OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE

# LOAD ENVIRONMENTAL VARIABLES
export FREESURFER_HOME=/fslhome/mpeter55/compute/research_bin/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.sh

# INSERT CODE, AND RUN YOUR PROGRAMS HERE
time=time5

~/compute/research_bin/freesurfer/bin/recon-all \
-subjid ${1} \
-i /fslhome/mpeter55/fsl_groups/fslg_autism_asym/compute/Autism_CSF_data/Auto_EACSF_ready/${time}_t1/${1}/anat/${1}_T1.nii.gz \
-wsatlas \
-all \
-sd /fslhome/mpeter55/compute/freesurfer_long/${time}/
