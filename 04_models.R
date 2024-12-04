library(readr)
library(tidyr)
library(brms)
library(ggplot2)
library(cmdstanr)
library(tidybayes)
library(dplyr)
library(fields)

# Cluster plotting
# options(bitmapType='cairo')

# What levels are we modeling?
myvar <- 'extreme_roundedness'
# 2: voicing, roundedness
# 3: height, backness
# 4: extreme
# 5: position, manner
# 7: manner_voicing
# 8: extreme_roundedness
# 10: position_voicing

folder_data_derived <- 'data_derived'  # path to folder with derived .csv files

# 25% increase/decrease of odds expressed as log-odds ratio
upr_thresh <- log(1.25)
lwr_thresh <- log(1/1.25)

#############################
### Functions             ###
#############################
odds <- function(x) {return(x / (1 - x))}

#############################
### Load data             ###
#############################
phylo_vcv <- read_rds('data_derived/df-phylo.rds')

myPropVars <- read_csv('data/data.csv', na=c('')) %>%
  mutate(
    extreme_roundedness=ifelse(!is.na(extreme), paste(extreme, roundedness, sep='-'), NA),
    manner_voicing=ifelse(!is.na(voicing), paste(manner, voicing, sep='-'), NA),
    position_voicing=ifelse(!is.na(voicing), paste(position, voicing, sep='-'), NA)
    ) %>% 
  mutate_if(is.character, factor) %>% 
  pull(myvar) %>% levels()

n_levels <- length(myPropVars)

data <- read_rds(paste0('data/processed_', myvar, '.rds', na=c(''))) %>% 
  # Some languages have the exact same coordinates!
  filter(!language %in% c('pana1310', 'yaga1256', 'sher1256')) %>% 
  # Dialect-level data makes problems with glottolog matrix
  filter(!language %in% c(
    'chim1313', 'vedi1234', 'mike1243', 'zafi1234',
    'anta1259', 'anta1260', 'anta1261', 'anta1262'
  ))

# Adapted from Hedvig, who based this on code from Sam Passmore
languages <- data %>%
  distinct(language, longitude, latitude) %>% 
  mutate(long_lat=paste0(longitude,"_", latitude)) %>% 
  mutate(dup=duplicated(long_lat) + duplicated(long_lat, fromLast=TRUE) ) %>% 
  mutate(longitude=ifelse(dup > 0, jitter(longitude, factor=2), longitude)) %>% 
  mutate(latitude=ifelse(dup > 0, jitter(latitude, factor=2), latitude))

# rgrambank, vcv
library(rgrambank)
coords <- languages %>% dplyr::select(longitude, latitude) %>%as.matrix()
kappa=2 # smoothness parameter as recommended by Dinnage et al. (2020)
sigma=c(1, 1.15) # Sigma parameter. First value is not used. 
spatial_vcv <- varcov.spatial.3D(coords=coords, cov.pars =sigma, kappa=kappa)$varcov
dimnames(spatial_vcv) <- list(languages$language, languages$language)

data2 <-  list(phylo_vcv=phylo_vcv, spatial_vcv=spatial_vcv)

#############################
### Priors                ###
#############################
priors_in <- list(
  intercepts=lapply(2:n_levels, function(i) {
    prior(normal(0, 1), class=Intercept, dpar='Intercept')}),
  sd=lapply(2:n_levels, function(i) {
    prior(gamma(3, 30), class=sd, dpar='sd')})#,
  #sdgp=lapply(2:n_levels, function(i) {
  #  prior(gamma(3, 30), class=sdgp, dpar='sdgp')})
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

# get_prior(
#   data=data,
#   data2=data2,
#   family='dirichlet',
#   formula=respDir ~ 1 + (1|concept) + 
#     (1 | gr(family, cov=phylo_vcv)) +
#     (1 | gr(language, cov=spatial_vcv)) 
# )

#############################
### Model                 ###
#############################
mod <- data %>%
  mutate(spatial_id=language, phylo_id=language) %>% 
  brm(
   data2=data2,
   family='dirichlet',
   formula=respDir ~ 1 + (1|concept) + 
     (1 | gr(phylo_id, cov=phylo_vcv)) +
     (1 | gr(spatial_id, cov=spatial_vcv)),
   prior=priors,
   silent=0,
   backend='cmdstanr',
   control=list(adapt_delta=0.85, max_treedepth=10),
   file=paste0('models/repl2024_lb2_', myvar, '.rds'),
   threads=threading(4),
   iter=5000, warmup=2500, chains=4, cores=16
   )

#############################
### Posterior predictions ###
#############################
new_data <- data %>% distinct(concept)
new_data <- tibble(concept=unique(data$concept), family='a', language='a')
fit_name <- paste0(folder_data_derived, '/repl2024_fit_', myvar, '.rds')
if (file.exists(fit_name)) {
  predictions <- readRDS(file=fit_name)
} else{
  print('Sorry, the file does not yet exist. This may take some time.')
  #predictions <- posterior_epred(newdata=new_data, mod, allow_new_levels=T)
  predictions <- add_epred_draws(newdata=new_data, mod, allow_new_levels=T)
  saveRDS(predictions, file=fit_name)
}

# Compute odds ratio for all fits based on odds for each category
preds_or <- predictions %>% group_by(.category) %>%
  mutate(
    OR_cat=odds(mean(.epred)),
    OR_word=log(odds(.epred)/OR_cat)
    ) %>% 
  ungroup() %>% 
  mutate(category=myPropVars[.category]) %>% 
  group_by(concept, category) %>% 
  # Summarise per word in each category
  summarise(mean=mean(OR_word), sd=sd(OR_word)) %>% 
  mutate(lwr=mean-2*sd, upr=mean+2*sd) %>%
  # Add Label for words above/below threshold
  mutate(word_dim=ifelse((lwr > upr_thresh | upr < lwr_thresh), toupper(as.character(concept)), ''))

#############################
### Plotting              ###
#############################
epred_plot <- preds_or %>% 
  ggplot(aes(x=concept, y=mean, ymin=lwr, ymax=upr, label=word_dim)) +
  geom_point() +
  geom_errorbar(width=0) +
  geom_text(size=3, nudge_x=3) +
  geom_point(aes(x=concept, y=mean), shape=4) +
  scale_x_discrete(labels=NULL, expand=c(0.02, 0.02)) +
  xlab('Concept') +
  ylab('Proportion, %') +
  coord_flip() +
  facet_wrap(~category, ncol=n_levels) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position='none')

ggsave(filename=paste0('figures/fit_', myvar, '.png'), epred_plot)
write_csv(preds_or, paste0(folder_data_derived, '/', myvar, '.csv'))
