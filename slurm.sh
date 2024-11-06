#!/bin/bash
#SBATCH --cpus-per-task 16
#SBATCH --mem 120G
#SBATCH -J results_ala
#SBATCH --partition dlcegpu
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err

srun Rscript --no-save --no-restore --verbose 02_preprocess.R
srun Rscript --no-save --no-restore --verbose 03_phylogeny.R
