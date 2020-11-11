#README File for Preprocessing Scripts

# Purpose of preproc: correct bias field inhomogeneities, resample to 1.0mm isotropic
#Order of execution:
#1. preproc_parallel_batch.sh This script calls ANTs N4BiasFieldCorrection to correct inhomogeneities. This is a "job" script and is set up for SLURM.
#2. time_preproc_wrap.sh This script is the wrapper script for #1 and submits jobs for each image in a subject ID text list. Set up for sbatch.
#3. preproc_parallel_batch_pt2.sh This script calls on c3d to resample each image. This is a "job" script and is set up for SLURM.
#4. pt2_wrap.sh This script is the wrapper script for #3 and submits a job for each image in a subject ID text list. Set up for sbatch.


