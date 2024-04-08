#!/bin/bash
#SBATCH --cpus-per-task 2
#SBATCH --mem 120G
#SBATCH -J replic
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode02

xvfb-run Rscript --verbose 02_models_gg.R
