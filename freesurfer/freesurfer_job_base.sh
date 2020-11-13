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

subjid=$1
export SUBJECTS_DIR=/fslhome/mpeter55/compute/HCP_trials/All_HCP_Data/freesurfer_2/consolidated/${1}


# INSERT CODE, AND RUN YOUR PROGRAMS HERE
~/compute/research_bin/freesurfer/bin/recon-all \
-subjid $1 \
-base base_${1} \
-tp MPR1 \
-tp MPR2 \
-all 
