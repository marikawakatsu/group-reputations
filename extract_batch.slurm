#!/bin/bash
#SBATCH --job-name=groupreps
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=13
#SBATCH --mem=16G
#SBATCH --time=02:00:00
#SBATCH --mail-type=all
#SBATCH --mail-user=sm75@princeton.edu
#SBATCH --output=./out/%A-%a.out

module purge
module load julia/1.5.0
module list

julia extract_simulation.jl
