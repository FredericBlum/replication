#!/bin/bash
#SBATCH --cpus-per-task 16
#SBATCH --mem 500G
#SBATCH -J repl
#SBATCH --output=%j.%x.out
#SBATCH --error=%j.%x.err

srun Rscript --no-save --no-restore --verbose -p standard 04_models.R 
