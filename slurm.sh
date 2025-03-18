#!/bin/bash
#SBATCH --cpus-per-task 16
#SBATCH --mem 20G
#SBATCH -J repl
#SBATCH --output=%j.%x.out
#SBATCH --error=%j.%x.err
#SBATCH --time=28-00:00:00

module load R/4.4.2
srun Rscript --no-save --no-restore --verbose 04_models.R -p=standard 
