#!/bin/bash
#SBATCH -w dlcenode01
#SBATCH --cpus-per-task 16
#SBATCH --mem 100G
#SBATCH -J repl
#SBATCH --output=%j.%x.out
#SBATCH --error=%j.%x.err
#SBATCH --time=28-00:00:00

module load R/4.4.2
srun Rscript --no-save --no-restore --verbose 03_models.R
