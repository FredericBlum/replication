library(readr)
library(tidyr)
library(brms)
library(ggplot2)
library(cmdstanr)
library(tidybayes)
library(dplyr)
library(matrixcalc) # check positive-definiteness

# What levels are we modeling?
args = commandArgs(trailingOnly=TRUE)

myvar <- args[1]
# 2: voicing, roundedness
# 3: height, backness
# 4: extreme
# 5: position, manner
# 7: manner_voicing
# 8: extreme_roundedness
# 10: position_voicing

folder_posterior_draws <- 'posterior_draws'

# 25% increase/decrease of odds expressed as log-odd s ratio
upr_thresh <- log(1.25)
lwr_thresh <- log(1/1.25)

#############################
### Functions             ###
#############################
odds <- function(x) {return(x / (1 - x))}

#############################
### Load data             ###
#############################
data <- read_rds(paste0('data/processed_', myvar, '.rds', na=c('')))

phylo_vcv <- read.csv('../01_preprocessing/data/vcv_phylo.csv', row.names=1) %>% as.matrix()
isSymmetric.matrix(phylo_vcv)

geo_vcv <- read.csv('../01_preprocessing/data/vcv_geo.csv', row.names=1) %>% as.matrix()
is.positive.definite(geo_vcv)

data2 <-  list(
  phylo_vcv=phylo_vcv,
  geo_vcv=geo_vcv
)

myPropVars <- read_csv('../01_preprocessing/data/data.csv', na=c('')) %>%
  mutate(
    extreme_roundedness=ifelse(!is.na(extreme), paste(extreme, roundedness, sep='-'), NA),
    manner_voicing=ifelse(!is.na(voicing), paste(manner, voicing, sep='-'), NA),
    position_voicing=ifelse(!is.na(voicing), paste(position, voicing, sep='-'), NA)
    ) %>% 
  mutate_if(is.character, factor) %>% 
  pull(myvar) %>% levels()

n_levels <- length(myPropVars)


#############################
### Priors                ###
#############################
get_prior(
  data=data,
  family='dirichlet',
  formula=respDir ~ 1 + (1|concept)
)

#############################
### Model                 ###
#############################
mod <- data %>%
  mutate(spatial_id=language, phylo_id=language) %>% 
  brm(
   family='dirichlet',
   formula=respDir ~ 1 + (1|concept),
   silent=0,
   backend='cmdstanr',
   control=list(adapt_delta=0.85, max_treedepth=10),
   file=paste0('models/noC_', myvar, '.rds'),
   threads=threading(4),
   iter=7500, warmup=2500, chains=4, cores=4
   )

priors_in <- list(
  intercepts=lapply(2:n_levels, function(i) {
    prior(normal(0, 0.5), class=Intercept, dpar='Intercept')}),
  sd=lapply(2:n_levels, function(i) {
    prior(gamma(3, 40), class=sd, dpar='sd')})
)

priors <- c(prior(gamma(1, 1), class=phi))
for (l in 1:length(priors_in)) {
  list <- priors_in[[l]]
  for (i in 1:length(list)) {
    j <- i + 1
    list[[i]]$dpar <- paste0('mu', j)
    priors <- c(priors, list[[i]])
  }
}

mod <- data %>%
  mutate(spatial_id=language, phylo_id=language) %>% 
  brm(
    data2=data2,
    family='dirichlet',
    formula=
      respDir ~ 1 + (1|concept) + 
      (1 | gr(phylo_id, cov=phylo_vcv)
      ),
    prior=priors,
    silent=0,
    backend='cmdstanr',
    control=list(adapt_delta=0.85, max_treedepth=10),
    file=paste0('models/phyloC_', myvar, '.rds'),
    threads=threading(4),
    iter=7500, warmup=2500, chains=4, cores=4
  )


mod <- data %>%
  mutate(spatial_id=language, phylo_id=language) %>% 
  brm(
    data2=data2,
    family='dirichlet',
    formula=
      respDir ~ 1 + (1|concept) + 
      (1 | gr(spatial_id, cov=geo_vcv)
      ),
    prior=priors,
    silent=0,
    backend='cmdstanr',
    control=list(adapt_delta=0.85, max_treedepth=10),
    file=paste0('models/areaC_', myvar, '.rds'),
    threads=threading(4),
    iter=7500, warmup=2500, chains=4, cores=4
  )
