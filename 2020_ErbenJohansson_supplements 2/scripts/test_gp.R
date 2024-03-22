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

