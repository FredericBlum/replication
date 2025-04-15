library(dplyr)
library(ggplot2)
library(ggrepel)
library(purrr)
library(readr)
library(viridis)

library(tidyr)
library(xtable)


## Load data
soundClasses <- c(
  'backness', 'height', 'roundedness', 'extreme', 'extreme_roundedness',
  'voicing', 'manner', 'manner_voicing', 'position', 'position_voicing'
  )

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
orig <- read_tsv('original_results_mapped.csv') %>% 
  select(word, group, fit, lwr, upr, topCardinal, myvar) %>% 
  rename(mean=fit, category=group, concept=word) %>% 
  mutate(result='Original Results', sd=NA, word_dim=NA,
         # Outcome is Strong if HPDI outside of ROPE
         # Outcome is Weak if Mean outside of ROPE and HPDI doesn't include 0
         # Outcome is Absent if HPDI fully inside ROPE
         outcome=ifelse((lwr>upr_thresh | upr < lwr_thresh), 'Strong', ifelse(
           (lwr > -lwr_thresh & upr < upr_thresh), 'No', ifelse(
             ((lwr > 0 & mean > upr_thresh) | (upr < 0 & mean < lwr_thresh)), 'Weak', 'Doubtful'))),
         label=ifelse(outcome=='Strong', paste0(toupper(concept), ' [', category, ']'), NA)
  )

combined <- df %>% 
  mutate(
    result='New Results', topCardinal='',
    label=paste0(toupper(concept), ' [', category, ']'),
    outcome=ifelse((lwr>upr_thresh | upr < lwr_thresh), 'Strong', ifelse(
      (lwr > -lwr_thresh & upr < upr_thresh), 'No', ifelse(
        ((lwr > 0 & mean > upr_thresh) | (upr < 0 & mean < lwr_thresh)), 'Weak', 'Doubtful')
      ))) %>%
  rbind(orig) %>% 
  mutate(
    concept=toupper(concept),
    result=factor(result, levels=c("Original Results","New Results"))
    ) %>% 
  filter(
    outcome %in% c('Strong', 'Weak'),
    # Remove negative values of binary outcomes
    !(myvar %in% c('voicing', 'roundedness') & mean < 0)
    )


################################################################################################
# Table stuff for paper
combined %>% select(-word_dim, -topCardinal) %>% write_csv(, file='data/final_results.csv')

results_table <- combined %>% group_by(myvar, result, outcome) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(result, outcome), values_from = n, values_fill=0)

colnames(results_table) <- c("Category", "Original_strong", "Original_weak", "New_weak", 'New_strong')
results_table <- results_table %>%
  select(Category, Original_strong, New_strong, Original_weak, New_weak) %>% 
  add_row(Category = "Total", summarise(., across(where(is.numeric), sum)))

print(xtable(results_table), type = "latex", include.rownames=FALSE)


################################
# Highest results
combined %>% filter(concept %in% c('DUST', 'TASTE')) %>% 
  arrange(concept, myvar, result, outcome)



# Plot for each sound class
for (sc in soundClasses) {
  test <- combined %>%
    filter(myvar == sc) %>% 
    ggplot(aes(
      x=mean, y=paste0(concept, category),
      xmin=lwr, xmax=upr,
      color=outcome, size=outcome
      )) +
    geom_errorbar(linewidth=1.5, width=0) +
    geom_point(aes(fill=outcome), shape=21) +
    geom_vline(xintercept=0, color='red') +
    geom_label_repel(aes(label=label), max.overlaps=99, size=5, box.padding=0.9) +
    annotate('rect', xmin=lwr_thresh, xmax=upr_thresh, ymin=0, ymax=Inf, alpha=.1) +
    facet_wrap( ~ result, ncol=2, drop=F) +
    scale_x_continuous(
      name=NULL,
      limits=c(-1.4, 1.4),
      breaks=seq(-1, 1, by=0.5),
      labels=seq(-1, 1, by=0.5)) +
    scale_y_discrete(name=NULL, expand = c(.03, .03)) +
    scale_size_manual(name = '', values=c(6, 4)) +
    scale_color_manual(name = '', values=c('Weak'=viridis(10)[3], 'Strong'=viridis(10)[8])) +
    scale_fill_manual(name = '', values=c('Weak'=viridis(10)[3], 'Strong'=viridis(10)[8])) +
    theme_bw() +
    theme(
      panel.spacing = unit(0.5, "lines"),
      panel.grid.major.y = element_blank() ,
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.x= element_text(size=20),
      strip.text.x = element_text(size=30),
      legend.position='none'
      )
  ggsave(filename=paste0('figures/summary_', sc, '.pdf'), dpi=500, height=15, width=7)
}
