#!/bin/bash
#SBATCH --cpus-per-task 4
#SBATCH --mem-per-cpu 20G
#SBATCH -J replic
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode02

Rscript --verbose 01_formatting.R
