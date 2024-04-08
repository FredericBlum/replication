library(tidyr)

post_model <- posterior_samples(mod)
vars <- variables(mod) %>% as_tibble()


post_model %>% 
  transmute(iter  = 1:n(),
            etasq = sdgp_mu2_gplongitudelatituderegionnorth_america^2,
            rhosq = lscale_mu2_gplongitudelatituderegionnorth_america^2 * .5) %>% 
  sample_n(80) %>% 
  expand(nesting(iter, etasq, rhosq),
         x = seq(from = 0, to = 205, by = 1)) %>% 
  mutate(covariance = etasq * exp(-rhosq * x^2)) %>% 
  
  # plot
  ggplot(aes(x = x, y = covariance)) +
  geom_line(aes(group = iter),
            linewidth = 1/4, alpha = 1/4, color = "#EEDA9D") +
  stat_function(fun = function(x) median(post_model$sdgp_mu2_gplongitudelatituderegionnorth_america)^2 *
                  exp(-median(post_model$lscale_mu2_gplongitudelatituderegionnorth_america)^2 *.5 * x^2),
                color = "#EEDA9D", linewidth = 1.1) +
  scale_x_continuous("distance (thousand km)", expand = c(0, 0),
                     breaks = seq(from = 0, to = 200, by = 50))


# Nice plot with spatial covariance


# needs islandsDistMatrix, for all languages of continent

# compute posterior median covariance among societies
k <- matrix(0, nrow = 10, ncol = 10)
for (i in 1:10)
  for (j in 1:10)
    k[i, j] <- median(post_b13.7$sdgp_gplatlon2^2) * 
  exp(-median(post_b13.7$lscale_gplatlon2^2) * 
        islandsDistMatrix[i, j]^2)

diag(k) <- median(post_b13.7$sdgp_gplatlon2^2) + 0.01

k %>% round(2)

# convert to correlation matrix
rho <- round(cov2cor(k), 2)

# add row/col names for convenience
colnames(rho) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
rownames(rho) <- colnames(rho)

rho %>% round(2)


# drop logpop
tidy_rho %>%       
  ggplot(aes(x = lon2, y = lat)) +
  geom_line(aes(group = group, alpha = correlation^2),
            color = "#80A0C7") +
  geom_point(data = d, aes(size = logpop), color = "#DCA258") +
  geom_text_repel(data = d, aes(label = culture), 
                  seed = 0, point.padding = .3, size = 3, color = "#FCF9F0") +
  scale_alpha_continuous(range = c(0, 1)) +
  labs(x = "longitude",
       y = "latitude") +
  coord_cartesian(xlim = range(d$lon2),
                  ylim = range(d$lat)) +
  theme(legend.position = "none") +
  theme_pearl_earring 
