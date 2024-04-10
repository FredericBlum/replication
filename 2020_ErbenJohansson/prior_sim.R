library(readr)
library(dplyr)
library(ggplot2)
library(ggdist)
library(viridis)

n <- 1e5

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

rcauchy(n, location=0, scale=0.5) %>% 
  tibble() %>% 
  mutate(group = 'Intercept%~% cauchy(0, 0.5)') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill = group)) +
  scale_y_continuous(breaks = NULL,
                     name = "Density of values") +
  scale_x_continuous(
    breaks = seq(from = -4, to = 4, by = 1),
    limits = c(-4, 4),
    name = "") +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +  
  labs(title = "Intercept ~ cauchy(0, 0.5)")

rexp(n, rate = 4) %>% 
  tibble() %>% 
  mutate(group = 'sigma%~% exp(5)') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill = group)) +
  scale_y_continuous(breaks = NULL,
                     name = "Density of values") +
  scale_x_continuous(#breaks = seq(from = 0, to = 1.2, by = 0.2),
    #limits = c(0, 1.2),
    name = "Standard deviation of varying intercepts on log-scale") +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +  
  labs(title = "σ ~ Exp(5)")

tibble(x = c(rnorm(n, 0, 1))) %>%
  mutate(group = 'beta%~% Normal(0, 1)') %>% 
  ggplot(aes(x = x)) +
  geom_density(aes(fill = group)) +
  scale_fill_viridis(discrete = T, alpha = 0.7, end = 0.7) +
  scale_y_continuous(breaks = NULL) +
  ylab("Density of values") +
  scale_x_continuous(name = "Predictor values on log-scale", 
                     limits = c(-4, 4), 
                     breaks = seq(from = -4, to = 4, by = 1)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14)) +
  labs(title = "β ~ Normal(0, 1)")

