#!/bin/bash

#SBATCH --time=00:10:00   # walltime
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

#subjid=$1
export SUBJECTS_DIR=/fslhome/mpeter55/compute/HCP_trials/All_HCP_Data/freesurfer_2/consolidated


# INSERT CODE, AND RUN YOUR PROGRAMS HERE

#asegstats2table --qdec-long long.qdec.table.dat --stats MPR1.long.base_${1}/stats/aseg.stats --tablefile aseg.table.csv
#asegstats2table --subjects $1/MPR2.long.base_${1} --meas volume --tablefile aseg_stats_MPR1.csv

asegstats2table --subjects 103818/MPR1.long.base_103818 105923/MPR1.long.base_105923 115320/MPR1.long.base_115320 122317/MPR1.long.base_122317 130518/MPR1.long.base_130518 137128/MPR1.long.base_137128 139839/MPR1.long.base_139839 143325/MPR1.long.base_143325 146129/MPR1.long.base_146129 149337/MPR1.long.base_149337 149741/MPR1.long.base_149741 151526/MPR1.long.base_151526 158035/MPR1.long.base_158035 169343/MPR1.long.base_169343 172332/MPR1.long.base_172332 177746/MPR1.long.base_177746 192439/MPR1.long.base_192439 194140/MPR1.long.base_194140 195041/MPR1.long.base_195041 200109/MPR1.long.base_200109 200614/MPR1.long.base_200614 204521/MPR1.long.base_204521 287248/MPR1.long.base_287248 341834/MPR1.long.base_341834 433839/MPR1.long.base_433839 601127/MPR1.long.base_601127 627549/MPR1.long.base_627549 660951/MPR1.long.base_660951 --meas volume --tablefile aseg_stats_MPR1_1.txt

