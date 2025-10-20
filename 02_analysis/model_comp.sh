#!/bin/bash
#SBATCH -J repl
#SBATCH --output=%x.%j.full_out
#SBATCH --error=%x.%j.full_err
#SBATCH --partition=standard  
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=64G
#SBATCH --time=16-24:00:00
#
set -euo pipefail
: "${PARAM}"

module load R/4.5.1
Rscript 04_modelComp_full.R "${PARAM}"
