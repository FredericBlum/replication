library(loo)
library(brms)
library(tibble)

variables <- c(
  'voicing', 'roundedness', 'height', 'backness', 'extreme', 'position', 'manner',
  'manner_voicing', 'extreme_roundedness', 'position_voicing'
  )

for var in variables {
  path <- 'models/'
  
  # Load models
  with_c <- readRDS(paste0(path, var, '.rds'))
  no_c <- readRDS(paste0(path, 'noC_', var, '.rds'))
  phylo_c <- readRDS(paste0(path, 'phyloC_', var, '.rds'))
  area_c <- readRDS(paste0(path, 'areaC_', var, '.rds'))
  
  # Compute LOO
  loo_noC <- loo(no_c, cores=10)
  loo_C <- loo(with_c, cores=10)
  loo_phyloC <- loo(phylo_c, cores=10)
  loo_areaC <- loo(area_c, cores=10)
  
  # Write comparison to file
  comp <- loo_compare(loo_C, loo_noC, loo_phyloC, loo_areaC)
  write.table(tibble(comp), paste0('models/comp_', var), sep="\t", quote=FALSE, row.names=FALSE)
}
