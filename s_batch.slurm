#!/bin/bash
#SBATCH --job-name=groupreps
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=13
#SBATCH --mem=16G
#SBATCH --time=6:00:00
#SBATCH --mail-type=all
#SBATCH --mail-user=sm75@princeton.edu
#SBATCH --array=1-6
#SBATCH --output=./out/%A-%a.out

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module purge
module load julia/1.5.0
module list

julia s_sweep_src-prob-rate.jl
