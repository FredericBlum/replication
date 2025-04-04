#!/bin/bash
#SBATCH --cpus-per-task 4
#SBATCH --mem 10G
#SBATCH -J repl
#SBATCH --output=%j.%x.out
#SBATCH --error=%j.%x.err
#SBATCH --time=1-00:00:00

module load R/4.4.2
srun Rscript --no-save --no-restore --verbose 02_preprocess.R --p=standard
