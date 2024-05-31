library(dplyr)
library(ggplot2)
library(ggrepel)
library(purrr)
library(readr)
library(viridis)

## Load data
soundClasses <- c(
  'backness', 'height', 'roundedness', 'extreme', 'extreme_roundedness',
  'voicing', 'manner', 'manner_voicing', 'position', 'position_voicing'
  )

# Code gives parsing warning, but the df is loaded correctly and problems() is empty
myfiles <- paste0(soundClasses, '.csv')
df <- map_df(myfiles, ~ {
  new_file <- paste0('data_derived/', .x)
  if (file.exists(new_file)) {
    temp <- read_csv(new_file)
    temp$myvar <- gsub('\\.csv$', '', .x)
    return(temp)
  }})

upr_thresh <- log(1.25)
lwr_thresh <- log(1/1.25)

# Pre-Process dataframes
orig <- read_csv('original_results.csv') %>% 
  select(word, group, fit, lwr, upr, topCardinal, myvar) %>% 
  rename(mean=fit, category=group) %>% 
  mutate(result='Original Results', sd=NA, word_dim=NA,
         # Outcome is Strong if HPDI outside of ROPE
         # Outcome is Weak if Mean outside of ROPE and HPDI doesnt include 0
         # Outcome is Absent if HPDI fully inside ROPE
         outcome=ifelse((lwr>upr_thresh | upr < lwr_thresh), 'Strong', ifelse(
           (lwr > -lwr_thresh & upr < upr_thresh), 'No', ifelse(
             ((lwr > 0 & mean > upr_thresh) | (upr < 0 & mean < lwr_thresh)), 'Weak', 'Doubtful'))),
         label=ifelse(outcome=='Strong', paste0(toupper(word), ' [', category, ']'), NA)
  )

df_plot <- df %>% 
  # Remove negative values of binary, since they are just mirroring the positive results
  # filter(!(myvar %in% c('voicing', 'roundedness') & mean < 0)) %>% 
  # Add judgement of result strength
  mutate(
    result='New Results', topCardinal='',
    label=paste0(toupper(word), ' [', category, ']'),
    # Outcome is Strong if HPDI outside of ROPE
    # Outcome is Weak if Mean outside of ROPE and HPDI doesnt include 0
    # Outcome is Absent if HPDI fully inside ROPE
    outcome=ifelse((lwr>upr_thresh | upr < lwr_thresh), 'Strong', ifelse(
      (lwr > -lwr_thresh & upr < upr_thresh), 'No', ifelse(
        ((lwr > 0 & mean > upr_thresh) | (upr < 0 & mean < lwr_thresh)), 'Weak', 'Doubtful')
      )
    )
  )

combined <- orig %>%
  rbind(df_plot) %>% 
  mutate(word=toupper(word)) %>% 
  filter(
    outcome %in% c('Strong', 'Weak'),
    # Remove negative values of binary outcomes
    !(myvar %in% c('voicing', 'roundedness') & mean < 0)
    ) 

write_csv(combined, file='data/final_results.csv')

# Plot for each sound class
for (sc in soundClasses) {
  combined %>%
    filter(myvar == sc) %>% 
    ggplot(aes(
      y=paste0(word, category), x=mean, xmin=lwr, xmax=upr,
      color=outcome, size=outcome)
      ) +
    geom_errorbar(linewidth=1.5, width=0) +
    geom_point(aes(fill=outcome), shape=21) +
    geom_vline(xintercept=0, color='red') +
    geom_label_repel(aes(label=label), max.overlaps=99, size=2.5, box.padding=0.6) +
    facet_wrap( ~ result, ncol=2) +
    # xlab('Odds ratio (0=chance, >0=overrepresented, <0=underrepresented)')
    ylab('') + xlab('') +
    scale_x_continuous(breaks=seq(-1.5, 1.24, by=0.5), labels=seq(-1.5, 1, by=0.5)) +
    scale_y_discrete(expand = c(.03, .03)) +
    scale_size_manual(name = '', values=c(5, 3)) +
    scale_color_manual(name = '', values=c('Weak'=viridis(10)[3], 'Strong'=viridis(10)[8])) +
    scale_fill_manual(name = '', values=c('Weak'=viridis(10)[3], 'Strong'=viridis(10)[8])) +
    theme_bw() +
    theme(
      panel.grid=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position='none'
      )
  
  ggsave(filename=paste0('figures/summary_', sc, '.png'),
         width=20,  height=15, units='cm', dpi=500)
}

# Plot for each sound class
combined %>%
  filter(result=='New Results') %>% 
  ggplot(aes(
    y=paste0(word, category), x=mean, xmin=lwr, xmax=upr,
    color=outcome, size=outcome)
  ) +
  geom_errorbar(linewidth=1.5, width=0) +
  geom_point(aes(fill=outcome), shape=21) +
  geom_vline(xintercept=0, color='red') +
  geom_label_repel(aes(label=word_dim), max.overlaps=99, size=2, box.padding=0.2) +
  facet_wrap( ~ myvar, ncol=5) +
  ylab('') + xlab('') +
  scale_x_continuous(breaks=seq(-1.5, 1.24, by=0.5), labels=seq(-1.5, 1, by=0.5)) +
  scale_y_discrete(expand = c(.03, .03)) +
  scale_size_manual(name = '', values=c(5, 3)) +
  scale_color_manual(name = '', values=c('Weak'=viridis(10)[3], 'Strong'=viridis(10)[8])) +
  scale_fill_manual(name = '', values=c('Weak'=viridis(10)[3], 'Strong'=viridis(10)[8])) +
  theme_bw() +
  theme(
    panel.grid=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position='none'
  )

ggsave(filename=paste0('figures/summary_plot.png'),
       width=30,  height=22, units='cm', dpi=500)
