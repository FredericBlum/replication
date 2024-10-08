library(brms)
library(ggplot2)
library(cmdstanr)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(tidybayes)
library(forcats)

library(geodist)


# Cluster plotting
options(bitmapType='cairo')
cores <-  parallel::detectCores()
chains <- 4

myvar <- 'roundedness'
# What levels are we modeling?
# 2: voicing, roundedness
# 3: height, backness
# 4: extreme
# 5: position, manner
# 8: extreme_roundedness
# 10: position_voicing, manner_voicing

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
data <- read_rds(paste0('data/processed_', myvar, '.rds', na=c('')))
phylo_vcv <- read_rds('data_derived/df-phylo.rds')


languages <- data %>% distinct(language, longitude, latitude) %>% 
  mutate(language=as.character(language))

distances <- languages %>% select(longitude, latitude) %>% 
  coords %>% 
  geodist(measure = "geodesic")

dimnames(distances) <- list(languages$language, languages$language)

library(Matrix)
pd_matrix <- nearPD(distances, corr = TRUE)$mat


# Check if matrix is positive definite
# 1
library(matrixcalc)
is.positive.definite(pd_matrix)
# 2
all(eigen(pd_matrix)$values > 0)


#data2 <-  list(phylo_vcv = phylo_vcv)#, spatial_vcv = spatial_vcv)
data2 <-  list(spatial_vcv = distances)

n_levels <- 2

#############################
### Priors                ###
#############################
priors_in <- list(
  intercepts = lapply(2:n_levels, function(i) {
    prior(normal(0, 1), class=Intercept, dpar='Intercept')}),
  sd = lapply(2:n_levels, function(i) {
    prior(gamma(3, 30), class=sd, dpar='sd')})#,
  #sdgp = lapply(2:n_levels, function(i) {
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

get_prior(
  data=data,
  data2=data2,
family='dirichlet',
formula=
  respDir ~
  1 + (1 | gr(language, cov = spatial_vcv)),# + #(1|concept)
  #gp(longitude, latitude, by=macroarea, gr=TRUE, scale=T)
)

#############################
### Model                 ###
#############################
word_var <- c('MOUTH', 'ROUND')
word <- data %>% filter(concept %in% word_var)

mod <- brm(
 data=word,
 data2=data2,
 family='dirichlet',
 formula=
   respDir ~
   1 + (1 | gr(family, cov = phylo_vcv)) + (1|concept) +
   #gp(longitude, latitude, gr=TRUE, scale=T),
   gp(longitude, latitude, by=macroarea, gr=TRUE, scale=T),
 prior=priors,
 silent=0,
 backend='cmdstanr',
 control=list(adapt_delta=0.80, max_treedepth=10),
 file=paste0('models/repl2024_lb2_sets_', myvar, '.rds'),
 threads=threading(1),
 iter=1000, warmup=500, chains=4, cores=4
 )

#############################
### Posterior predictions ###
#############################
new_data <- tibble(concept=word_var, latitude=0, longitude=-160, family='a', language='a')
fit_name <- paste0(folder_data_derived, '/repl2024_fit_', myvar, '.rds')
if (file.exists(fit_name)) {
  predictions <- readRDS(file=fit_name)
} else{
  print('Sorry, the file does not yet exist. This may take some time.')
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
