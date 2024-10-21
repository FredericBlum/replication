library(readr)
library(tidyr)
library(dplyr)

# Attempts of problem-solving
library(matrixcalc) # check PD

#############################
### Load data             ###
#############################
myvar <- 'roundedness'
data <- read_rds(paste0('data/processed_', myvar, '.rds', na=c(''))) %>% 
  # Some languages have the exact same coordinates!
  filter(!language %in% c('pana1310', 'yaga1256', 'sher1256'))

# Adapted from Hedvig, who based this on code from Sam Passmore
languages <- data %>%
  distinct(language, longitude, latitude) %>% 
  mutate(long_lat = paste0(longitude,"_", latitude)) %>% 
  mutate(dup = duplicated(long_lat) + duplicated(long_lat, fromLast = TRUE) ) %>% 
  mutate(longitude = ifelse(dup > 0, jitter(longitude, factor = 2), longitude)) %>% 
  mutate(latitude = ifelse(dup > 0, jitter(latitude, factor = 2), latitude))

# Check if no exact matches keep existing
languages %>% group_by(longitude, latitude) %>% 
  summarize(count=n(), .groups='drop') %>% 
  right_join(languages, by=c('longitude', 'latitude')) %>% 
  filter(count==2)

# rgrambank, vcv
library(rgrambank)
coords <- languages %>% 
  column_to_rownames("language") %>% 
  dplyr::select(longitude, latitude) %>% 
  as.matrix()

kappa = 2 # smoothness parameter as recommended by Dinnage et al. (2020)
sigma = c(1, 1.15) # Sigma parameter. First value is not used. 

spatial_vcv <- varcov.spatial.3D(coords=coords, cov.pars =sigma, kappa=kappa)$varcov
dimnames(spatial_vcv) <- list(languages$language, languages$language)

is.positive.definite(spatial_vcv)

# Correlation matrix instead?
cor_matrix <- cor(spatial_vcv)
dimnames(cor_matrix) <- list(languages$language, languages$language)

is.positive.definite(cor_matrix)

# Check large values
library(reshape2)

# Convert to long format
cor_long <- melt(cor_matrix)

# Filter out self-correlations (diagonal)
cor_long <- cor_long[cor_long$Var1 != cor_long$Var2, ]

# Get the largest values
largest_correlations <- cor_long[order(-abs(cor_long$value)), ][1:5, ]  # Change 5 to the number of top correlations you want
print(largest_correlations)


# Geodesic, my own approach
library(geodist) # geodesic()
distances <- languages  %>% 
  geodist(measure="geodesic") %>% 
  as.matrix()
dimnames(distances) <- list(languages$language, languages$language)
is.positive.definite(distances)

# Approach from Hedvig using fields
library(fields, attach.required=F)
coords <- languages %>% select(longitude, latitude) %>%  as.matrix()
dists_3D <- fields::rdist.earth(x1=coords, x2=coords, miles=F) 
dimnames(dists_3D) <- list(languages$language, languages$language)
is.positive.definite(dists_3D)
