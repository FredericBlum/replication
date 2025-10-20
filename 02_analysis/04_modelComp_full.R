library(loo)
library(brms)
library(tibble)

args = commandArgs(trailingOnly=TRUE)

myvar <- args[1]
options(mc.cores = 8)
# variables <- c(
#   'voicing', 'roundedness', 'height', 'backness', 'extreme', 'position', 'manner',
#   'manner_voicing', 'extreme_roundedness', 'position_voicing'
#   )

path <- 'models/'

# Load models

# Compute LOO
no_c <- readRDS(paste0(path, 'noC_', myvar, '.rds'))
loo_noC <- loo(no_c)
rm(no_c)
print('noC done')
with_c <- readRDS(paste0(path, myvar, '.rds'))
loo_C <- loo(with_c)
rm(with_c)

print('noC done')
phylo_c <- readRDS(paste0(path, 'phyloC_', myvar, '.rds'))
loo_phyloC <- loo(phylo_c)
rm(phylo_c)

print('noC done')
area_c <- readRDS(paste0(path, 'areaC_', myvar, '.rds'))
loo_areaC <- loo(area_c)
rm(area_c)

print('noC done')
# Write comparison to file
comp <- loo_compare(loo_C, loo_noC, loo_phyloC, loo_areaC)
print('until writing')
print(comp)
write.table(tibble(comp), paste0('models/compFull_', myvar), sep="\t", quote=FALSE) 
