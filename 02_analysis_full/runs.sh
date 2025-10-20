#!/bin/bash
#SBATCH -J repl
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition=standard  
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=6G
#SBATCH --time=20-24:00:00

set -euo pipefail
: "${PARAM}"

module load R/4.5.1 
Rscript 03_models.R "${PARAM}" 
