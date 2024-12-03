#!/bin/bash
#SBATCH --cpus-per-task 16
#SBATCH --mem 250G
#SBATCH -J repl
#SBATCH -w dlcenode01
#SBATCH --partition=dlcegpu
#SBATCH --output=%j.%x.out
#SBATCH --error=%j.%x.err

srun Rscript --no-save --no-restore --verbose 04_models.R 
