library(loo)
library(brms)
library(tibble)

args = commandArgs(trailingOnly=TRUE)

myvar <- args[1]

# variables <- c(
#   'voicing', 'roundedness', 'height', 'backness', 'extreme', 'position', 'manner',
#   'manner_voicing', 'extreme_roundedness', 'position_voicing'
#   )

path <- 'models/'

# Load models
with_c <- readRDS(paste0(path, myvar, '.rds'))
no_c <- readRDS(paste0(path, 'noC_', myvar, '.rds'))
phylo_c <- readRDS(paste0(path, 'phyloC_', myvar, '.rds'))
area_c <- readRDS(paste0(path, 'areaC_', myvar, '.rds'))

# Compute kfold cv
loo_noC <- kfold(no_c)
loo_C <- kfold(with_c)
loo_phyloC <- kfold(phylo_c)
loo_areaC <- kfold(area_c)


# Write comparison to file
comp <- loo_compare(loo_C, loo_noC, loo_phyloC, loo_areaC)
write.table(tibble(comp), paste0('models/comp_kfold_', myvar), sep="\t", quote=FALSE, row.names=FALSE)
