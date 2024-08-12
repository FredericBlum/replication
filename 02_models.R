library(brms)
library(ggplot2)
library(cmdstanr)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(tidybayes)
library(forcats)
library(chkptstanr)

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

countBy <- function(groupingVar, normBy, dataSource) {
  # Transform counts to proportions
  out <- dataSource %>%
    drop_na(groupingVar) %>% 
    # changing across() to all_of, lets see if this breaks things
    group_by(wd_id, language, word, concept, family, macroarea, latitude, longitude, across(groupingVar), across(all_of(normBy))) %>%
    count() %>% ungroup() %>%
    mutate(prop=n/c_across(normBy), vars=c_across(groupingVar)) %>% 
    select(language, wd_id, word, concept, family, macroarea, latitude, longitude, vars, prop) %>%
    pivot_wider(names_from=vars, values_from=prop)
  
  # Note: Since we are calculating proportions, this is reasonable!
  # Right now, NA is created when no phoneme for the grouping variable is present
  # But we actually want a 0 in those cases, to represent the proportion --> 0
  # And since Dirichlet needs response above 0, we need to modify slightly
  out[is.na(out)] <- 0.001

  # Make sure rows sum up to 1  
  for(i in 1:nrow(out)) {
    row <- out[i, 9:ncol(out)]
    m <- which.max(row[,])
    row[m] <- 1 - sum(row[-m])
    out[i,] <- cbind(out[i, 1:8], row)
  }
  
  # Create matrix for brms
  respDir <- as.matrix(out[, myPropVars])
  colnames(respDir) <- 1:ncol(respDir)
  out$respDir <- respDir
  out <- out[, c('language', 'concept', 'wd_id', 'word', 'family', 'macroarea', 'latitude', 'longitude', 'respDir')]

  return(out)
}

#############################
### Load data             ###
#############################
df <- read_csv('new_data/data.csv', na=c('')) %>%
  mutate_if(is.character, factor) %>%
  mutate(
    vowelConsonant=ifelse(!is.na(height), 'vowel', 'consonant'),
    family=fct_na_value_to_level(family, 'Isolate'),
    extreme_roundedness=paste(extreme, roundedness, sep='-'),
    manner_voicing=paste(manner, voicing, sep='-'),
    position_voicing=paste(position, voicing, sep='-')
  ) %>% 
  filter(!is.na(macroarea))

# Words that are uncommonly long/short
avg_length <- df %>% group_by(word) %>% summarise(mean=mean(nPhonemesPerWord))
avg_length %>% arrange(mean)
avg_length %>% arrange(-mean)

# Coverage
unique(df[c('concept', 'language')]) %>% group_by(concept) %>% count() %>% arrange(n)

# Subset data: for vowel features, focus only on vowels; etc.
if (myvar %in% c('manner', 'manner_voicing', 'position', 'position_voicing', 'voicing')) {
  mySounds <- 'consonants'
  df1 <- df %>% filter(vowelConsonant == 'consonant') %>% 
    group_by(wd_id) %>% 
    mutate(nConsPerWord=n()) %>% 
    ungroup()
  mv <- 'nConsPerWord'
} else if (myvar %in% c('height', 'backness', 'roundedness', 'extreme', 'extreme_roundedness')) {
  mySounds='vowels'
  df1 <- df %>% filter(vowelConsonant == 'vowel') %>% 
    group_by(wd_id) %>% 
    mutate(nVowelsPerWord=n()) %>% 
    ungroup() 
    mv <- 'nVowelsPerWord'
}

length(unique(df1$language))
length(unique(df1$family))
df1 %>% filter(family=='Isolate') %>% group_by(language) %>% count()

myPropVars <- df1 %>% pull(myvar) %>% levels()
n_levels <- length(myPropVars)

data <- countBy(groupingVar=myvar, normBy=mv, dataSource=df1) 

#############################
### Priors                ###
#############################
priors_in <- list(
  intercepts = lapply(2:n_levels, function(i) {
    prior(normal(0, 0.5), class=Intercept, dpar='Intercept')}),
  sd = lapply(2:n_levels, function(i) {
    prior(gamma(3, 30), class=sd, dpar='sd')}),
  sdgp = lapply(2:n_levels, function(i) {
    prior(gamma(3, 30), class=sdgp, dpar='sdgp')})
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
# family='dirichlet',
# formula=
#   respDir ~
#   1 + (1|concept) + (1|language) + (1|family) +
#   gp(longitude, latitude, gr=TRUE, by=macroarea, scale=F)
# )

#############################
### Model                 ###
#############################
mod <- chkpt_brms(
  data=data,
  formula=bf(
    family='dirichlet',
    formula=
        respDir ~
        1 + (1|concept) + (1|language) + (1|family) +
        gp(longitude, latitude, gr=TRUE, by=macroarea, scale=F)
        ),
  prior=priors,
  path='chkpt_folder/',
  silent=0,
  backend='cmdstanr',
  control=list(adapt_delta=0.85, max_treedepth=10),
  file=paste0('models/repl2024_', myvar, '.rds'),
  iter_sampling=5000,
  iter_warmup=2500,
  iter_per_chkpt=200,
  parallel_chains=4,
  brmsfit=TRUE,
  cores=cores,
  threads_per=cores/chains
  )

#mod <- brm(
#  data=data,
#  family='dirichlet',
#  formula=
#    respDir ~
#    1 + (1|concept) + (1|language) + (1|family) +
#    gp(longitude, latitude, gr=TRUE, by=macroarea, scale=F),
#  prior=priors,
#  silent=0,
#  backend='cmdstanr',
#  control=list(adapt_delta=0.85, max_treedepth=10),
#  file=paste0('models/repl2024_', myvar, '.rds'),
#  threads=threading(18),
#  iter=5000, warmup=2000, chains=4, cores=4
#  )

#############################
### Posterior predictions ###
#############################
new_data <- tibble(word=unique(data$concept), latitude=0, longitude=150)
fit_name=paste0(folder_data_derived, '/repl2024_fit_', myvar, '.rds')
if (file.exists(fit_name)) {
  predictions <- readRDS(file=fit_name)
} else{
  print('Sorry, the file does not yet exist. This may take some time.')
  predictions <- add_epred_draws(newdata=new_data, mod, re_formula='~(1|concept)')
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
  group_by(word, category) %>% 
  # Summarise per word in each category
  summarise(mean=mean(OR_word), sd=sd(OR_word)) %>% 
  mutate(lwr=mean-2*sd, upr=mean+2*sd) %>%
  # Add Label for words above/below threshold
  mutate(word_dim=ifelse((lwr > upr_thresh | upr < lwr_thresh), toupper(as.character(word)), ''))

#############################
### Plotting              ###
#############################
epred_plot <- preds_or %>% 
  ggplot(aes(x=word, y=mean, ymin=lwr, ymax=upr, label=word_dim)) +
  geom_point() +
  geom_errorbar(width=0) +
  geom_text(size=3, nudge_x=3) +
  geom_point(aes(x=word, y=mean), shape=4) +
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
