#!/bin/bash -l

#SBATCH -J REPL_SoundSymb
#SBATCH -o ./out.%j
#SBATCH -e ./err.%j
#SBATCH -D ./
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=50
#SBATCH --mem=50000MB
#SBATCH --time=24:00:00

module purge
module load R/4.3

srun Rscript --no-save --no-restore --verbose 02_models.R
