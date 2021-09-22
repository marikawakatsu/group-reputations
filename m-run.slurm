#!/bin/bash
#SBATCH --job-name=groupreps
#SBATCH --mem=16G
#SBATCH --time=24:00:00
#SBATCH --mail-type=all
#SBATCH --mail-user=mk28@princeton.edu
#SBATCH --array=325-648
#SBATCH --output=out/%A-%a.out

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module purge
module load julia/1.5.0
module list

julia sweep_0.jl
