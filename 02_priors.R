library(dplyr)
library(ggplot2)
library(ggdist)
library(viridis)

n <- 1e5

rnorm(n, mean=0, sd=0.5) %>% 
  tibble() %>% 
  mutate(group = 'Intercept%~% normal(0, 0.5)') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill = group)) +
  scale_y_continuous(breaks = NULL, name = "Density of values") +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  theme(legend.position = "none", plot.title = element_text(size = 14)) +  
  labs(title = "Intercept ~ normal(0, 0.5)")

rgamma(n, shape=1, rate=1) %>% 
  tibble() %>% 
  mutate(group = 'phi%~% gamma(1, 1)') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill = group)) +
  scale_y_continuous(breaks = NULL,
                     name = "Density of values") +
  scale_x_continuous(#breaks = seq(from = 0, to = 1.2, by = 0.2),
    #limits = c(0, 1.2),
    name = "") +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +  
  labs(title = "phi ~ gamma(1, 1)")

rgamma(n, shape=3, rate=30) %>% 
  tibble() %>% 
  mutate(group = 'sd%~% gamma(3, 30)') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill = group)) +
  scale_y_continuous(breaks = NULL, name = "Density of values") +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +  
  labs(title = "sd ~ gamma(3, 30)")
