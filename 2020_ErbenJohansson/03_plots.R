# Compiles a pooled table of fitted values from all models and plots them
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(viridis)

## Load data
soundClasses=c('vowelConsonant', 'backness', 'height', 'roundedness', 'extreme', 'extreme_roundedness', 'voicing', 'manner', 'manner_voicing', 'position', 'position_voicing')

myfiles <- paste0(soundClasses, '.csv')
df <- map_df(myfiles, ~ {
  new_file <- paste0('data_derived/', .x)
  if (file.exists(new_file)) {
    temp <- read_csv(new_file)
    temp$myvar <- gsub("\\.csv$", "", .x)
    return(temp)
  }})

write_csv(df, 'data_derived/models_complete.csv')

# simplify the labels of phoneme groups
ab <-  read_csv('data/sound_groups_abbreviation.csv')
ipa <-  read_csv('data/phonetic_groups.csv')

df$group_ab=ab$abbreviation[match(df$category, ab$sound_group)]

upr_thresh <- log(1.25)
lwr_thresh <- log(0.75)

# Strong observed results (regardless of fit values)

df_plot <- df %>% 
  mutate(
    outcome=ifelse((lwr>upr_thresh | upr < lwr_thresh), 'Strong', ifelse(
      (lwr > -lwr_thresh & upr < upr_thresh), 'No', ifelse(
        ((lwr > 0 & mean > upr_thresh) | (upr < 0 & mean < lwr_thresh)), 'Weak', 'Doubtful')
      )
    )
  ) %>% 
  filter(outcome %in% c('Strong', 'Weak'))
  

# Currently I do not have the main cardinal!
# idx=match(df_plot$topCardinal, ipa$cardinal)

hjust <- ifelse(df_plot$mean < 0, 2, 0)

df_plot %>%
  ggplot(aes(
    x=word, y=mean, ymin=lwr, ymax=upr,
    color=outcome, size=outcome
    )) +
  geom_errorbar(linewidth=1, width=0) +
  geom_point(aes(fill=outcome), shape=21) +
  geom_hline(yintercept=0, color='red') +
  geom_label(aes(label=word_dim), size=3, nudge_x=2, nudge_y=0.3) +
  facet_wrap( ~ myvar, ncol=5) +
  ylab('Odds ratio (0=chance, >0=overrepresented, <0=underrepresented)') +
  xlab('') +
  scale_y_continuous(breaks=seq(-1.5, 1.24, by=0.5), labels=seq(-1.5, 1, by=0.5)) +
  scale_color_viridis(discrete=TRUE, name='Type of outcome', begin=0.2, end=0.7) +
  scale_fill_viridis(discrete=TRUE, name='Type of outcome', begin=0.2, end=0.7) +
  scale_size_manual(name='Type of outcome', values=c(3, 1.5)) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
    )

ggsave(filename=paste0('pix_repl2024/summary', '.png'),
       width=40,  height=21, units='cm', dpi=300)
