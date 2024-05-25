#!/bin/bash
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH -J replic
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH -w dlcenode10

xvfb-run Rscript --verbose 02_models.R
