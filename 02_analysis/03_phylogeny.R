###############################################################################
# This code is only slightly adapted from the following publication:
# Guzmán Naranjo, M., & Becker, L. (2021). Statistical bias control in typology.
# Linguistic Typology, 26(3), 605–670. https://doi.org/10.1515/lingty-2021-0002

# We thank the authors for sharing their code, making it possible to re-use
# this method easily for new studies, and thus actively helping other scholars.
###############################################################################
library(ape)
library(readr)
library(dplyr)
library(stringr)

## functions

## this is the main function for getting the whole family tree for a language
get_chain <- function(.id) {
  cid <- .id
  pid <- get_parent(cid)
  chain <- c(.id, pid)
  while(!is.na(pid)) {
    pid <- get_parent(cid)
    if(!is.na(pid))
      chain <- c(chain, pid)
    cid <- pid
  }
  chain <- chain[!is.na(chain)]
  return(chain)
}

get_names <- function(.idf, db=glottolog)
  sapply(.idf, function(.id)
    db[db$id == .id, ]$name)
get_num_languages <- function(.idf, db=glottolog)
  sapply(.idf, function(.id)
    db[db$id == .id, ]$child_language_count)
get_num_families <- function(.idf, db=glottolog)
  sapply(.idf, function(.id)
    db[db$id == .id, ]$child_family_count)
get_parent <- function(.id, db=glottolog){
  db[db$id == .id, ]$parent_id
}

build_phylos <- function(.lfd, .var, .micro_family=FALSE, distance=FALSE) {
  .var <- enquo(.var)
  ## extract family chain
  chains <- sapply(.lfd$id,
                   function(x) {
                     print(x)
                     c(get_names(get_chain(x)), 'TOP__')
                   })
  ## get the family chains
  chain.1 <- chains %>% sapply(function(x)(paste(x, collapse=';')))
  if(.micro_family)
    chain.1 <- sapply(chain.1, function(x) str_remove(x, '^.*?;'))
  all.vals <- unique(unlist(strsplit(chain.1, ';')))
  all.vals <- all.vals[!is.na(all.vals)]
  ## build dataframes
  df.philo <- select(.lfd, !!.var) 
  for(col in all.vals){
    print(col)
    df.philo[,col] <- as.integer(str_detect(chain.1, col))
  }
  df.philo <- distinct(df.philo)
  df.philo_d <- dist.b(df.philo)
  if (distance) {
    df.philo_d
  } else {
    as.dist(1/df.philo_d) %>%
      hclust() %>%
      as.phylo()
  }
}

## distance function

dist.b <- function(X) {
  m <- as.matrix(as.data.frame(X)[,-1])
  rownames(m) <- as.data.frame(X)[,1]
  m <- cbind(1:nrow(m), m)
  apply(m, 1, function(r) {
    cat(r[1], '/', nrow(m), '- ', sep='')
    r[r==0] <- -1
    rowSums(t(t(m[,-1])==r[-1]))
  })
}


## build data from glottolog
glottolog <- read_csv('language.csv')
glottocodes <- glottolog %>% select(id, Family_id, Name)

fam_ids <- glottolog %>% pull(Family_id)
fam_codes <- glottolog %>% select(id, Name)

## we do a double left join to have the data in two columns
combined_data <-
  left_join(glottocodes, fam_codes, by=c('Family_id'='id'),
            suffix=c('_language', '_macro_family')) %>%
  mutate(name_macro_family =
           case_when(is.na(Name_macro_family) ~ Name_language, TRUE ~ Name_macro_family)) %>% 
  rename(Family=Name_macro_family)

# Run functions
myvar <- 'voicing'
langs <- read_rds(paste0('data/processed_', myvar, '.rds', na=c(''))) %>% distinct(language)
  
aff_phylo <- combined_data %>%
  filter(id %in% langs$language) %>% 
  build_phylos(id, .micro_family=FALSE)

ape::vcv(aff_phylo) %>% write_rds('data_derived/df-phylo.rds')
