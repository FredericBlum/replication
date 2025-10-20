library(loo)
library(brms)
library(tibble)

args=commandArgs(trailingOnly=TRUE)

myvar <- args[1]

# variables <- c(
#   'voicing', 'roundedness', 'height', 'backness', 'extreme', 'position', 'manner',
#   'manner_voicing', 'extreme_roundedness', 'position_voicing'
#   )

path <- 'models/'

# Load models
no_c <- readRDS(paste0(path, 'noC_', myvar, '.rds'))
# Compute LOO
set.seed(161)
N <- nobs(no_c)
idx <- if (N >= 2000) N else sample.int(N, max(50, floor(0.05*N)))
# Index -------------------

loo_noC<- brms::loo_subsample(
  no_c,
  observations=idx,
  ndraws=512,
  cores=8
)
rm(no_c)
print('no_c done')
area_c <- readRDS(paste0(path, 'areaC_', myvar, '.rds'))
loo_areaC<- brms::loo_subsample(
  area_c,
  observations=idx,
  ndraws=512,
  cores=8
)
rm(area_c)
print('area_c done')

phylo_c <- readRDS(paste0(path, 'phyloC_', myvar, '.rds'))
loo_phyloC<- brms::loo_subsample(
  phylo_c,
  observations=idx,
  ndraws=512,
  cores=8
)
rm(phylo_c)
print('phylo_c done')

with_c <- readRDS(paste0(path, myvar, '.rds'))
loo_withC<- brms::loo_subsample(
  with_c,
  observations=idx,
  ndraws=512,
  cores=8
)
rm(with_c)
print('with_c done')


comp <- loo_compare(loo_noC, loo_withC, loo_phyloC, loo_areaC)
print(comp)

# Write comparison to file
write.table(tibble(comp), paste0('models/comp_', myvar), sep="\t", quote=FALSE)
