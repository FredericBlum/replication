library(loo)
library(brms)
library(tibble)

args=commandArgs(trailingOnly=TRUE)

myvar <- args[1]
n_sample <- 1e6

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
idx <- if (N >= 2000) sample.int(N, n_sample) else sample.int(N, max(50, floor(0.05*N)))
# Index -------------------

loo_noC<- brms::loo_subsample(
  no_c,
  observations=idx,
  ndraws=256,
  cores=8
)
rm(no_c)

area_c <- readRDS(paste0(path, 'areaC_', myvar, '.rds'))
loo_areaC<- brms::loo_subsample(
  area_c,
  observations=idx,
  ndraws=256,
  cores=8
)
rm(area_c)

phylo_c <- readRDS(paste0(path, 'phyloC_', myvar, '.rds'))
loo_phyloC<- brms::loo_subsample(
  phylo_c,
  observations=idx,
  ndraws=256,
  cores=8
)
rm(phylo_c)

with_c <- readRDS(paste0(path, myvar, '.rds'))
loo_withC<- brms::loo_subsample(
  with_c,
  observations=idx,
  ndraws=256,
  cores=8
)
rm(with_c)


comp <- loo_compare(loo_noC_voicing, loo_withC_voicing, loo_phyloC_voicing, loo_areaC_voicing)
print(comp)

# Write comparison to file
write.table(tibble(comp), paste0('models/comp_', myvar), sep="\t", quote=FALSE, row.names=FALSE)
