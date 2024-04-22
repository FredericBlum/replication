# Compiles a pooled table of fitted values from all models and plots them

library(ggplot2)

## Load data
soundClasses = c('vowelConsonant', 'backness', 'height', 'roundedness', 'extreme', 'extreme_roundedness', 'voicing', 'manner', 'manner_voicing', 'position', 'position_voicing')
soundClasses1 = c('vowelConsonant', 'backness', 'height', 'roundedness', 'extreme', 'extreme-roundedness', 'voicing', 'manner', 'manner-voicing', 'position', 'position-voicing')

if (file.exists('data_derived/models_complete.csv')) {
  df = read.csv('data_derived/models_complete.csv')
} else {
  myfiles = paste0(soundClasses, '.csv')
  df = NULL
  for (f in 1:length(myfiles)) {
    new_file = paste0('data_derived/', myfiles[f])
    if (file.exists(new_file)) {
      temp = read.csv(new_file)
      temp$myvar = soundClasses[f]
      if (is.null(df)) {
        df = temp
      } else {
        df = rbind(df, temp)
      }
    }
  }
  write.csv(df, 'data_derived/models_complete.csv')
}
# head(df)

df$word = as.character(df$word)
df$word[df$word == 'nothing'] = 'no_thing'
df_word = as.factor(df$word)
df$word_group = paste0(df$word, '\n', df$group)
df$type = ifelse(df$fit > 0, 'positive', 'negative')

# simplify the labels of phoneme groups
ab = read.csv('data/sound_groups_abbreviation.csv')
df$group_ab = ab$abbreviation[match(df$group, ab$sound_group)]
# unique(df$group_ab)


#####
threshold = 25  # minimum change of odds ratio, in %
#####

threshold_log = log2(1 + threshold / 100)
sum(abs(df$obs_OR) > threshold_log)
mean(abs(df$obs_OR) > threshold_log)

# Strong observed results (regardless of fit values)
threshold_obs = 100  # 100%, or double the expected odds
df_obs = df[abs(df$obs_OR) > log2(1 + threshold_obs / 100), c('myvar', 'group', 'word_caps', 'obs_OR', 'fit', 'lwr', 'upr', 'nLangs', 'nRegions', 'cardinal', 'topCardinal')]
df_obs = df_obs[order(df_obs$myvar, df_obs$group, df_obs$word_caps), ]
# write.csv(df_obs, paste0('data_derived/summary_obs', threshold_obs, '.csv'), row.names = FALSE)


df1 = df
for (i in 1:nrow(df1)) {
  if (df1$lwr[i] > threshold_log | df1$upr[i] < -threshold_log) {  # outside ROPE
    df1$outcome[i] = 'Strong'
  } else if (df1$lwr[i] > -threshold_log & df1$upr[i] < threshold_log) {  # inside ROPE
    df1$outcome[i] = 'No'
  } else {  # overlaps with ROPE
    if ((df1$lwr[i] * df1$upr[i] > 0) & abs(df1$fit[i]) > threshold_log) {  # CI excludes 0 and fit outside ROPE
      df1$outcome[i] = 'Weak'
    } else {
      df1$outcome[i] = 'Doubtful'
    }
  }
}

# drop negatives for binary groups b/c they are just mirror images of the positives
df1 = droplevels(df1[!(df1$myvar %in% c('voicing', 'roundedness', 'vowelConsonant') &
                         df1$type == 'negative'), ])
# exclude vowel-consonant model
df1 = droplevels(df1[df1$myvar != 'vowelConsonant', ])

df1$outcome = factor(df1$outcome, levels = c('Strong', 'Weak', 'Doubtful', 'No'))
# table(df1$outcome)
# table(df1$outcome) / nrow(df1) * 100

## plotting
df_plot = droplevels(df1[df1$outcome %in% c('Strong', 'Weak'), ])
df_plot$word_caps = toupper(df_plot$word)
df_plot$word_group = paste(df_plot$word_caps, df_plot$group_ab)

# change unicode to ipa for "cardinal"
ipa = read.csv('data/phonetic_groups.csv', stringsAsFactors = FALSE)
idx = match(df_plot$topCardinal, ipa$cardinal)
# any(is.na(idx))
df_plot$topCardinal_ipa = ipa$cardinal_ipa[idx]
for (i in 1:nrow(df_plot)) {
  c = as.character(df_plot$cardinal[i])
  c1 = unlist(strsplit(c, ''))
  for (j in 1:length(c1)) {
    c1[j] = ipa$cardinal_ipa[ipa$cardinal == c1[j]][1]
  }
  df_plot$cardinal_ipa[i] = paste0(c1, collapse = '')
}

# prepare plot labels; replace NA cardinal with /-/
df_plot$cardinal_ipa[df_plot$cardinal_ipa == 'NA'] = '-'
df_plot$cardinal_ipa[df_plot$myvar == 'vowelConsonant'] = ''  # too many to display
df_plot$topCardinal_ipa[is.na(df_plot$topCardinal_ipa)] = '-'
for (i in 1:nrow(df_plot)) {
  if (df_plot$cardinal_ipa[i] == '-') {
    df_plot$mylabel[i] = paste0(df_plot$word_group[i], ' (-)')
  } else {
    df_plot$mylabel[i] = paste0(
      df_plot$word_group[i], ' (', df_plot$topCardinal_ipa[i], ')')
  }
}
df_plot$pos = ifelse(df_plot$type == 'positive', max(df_plot$upr), min(df_plot$lwr))
df_plot$myvar = as.character(df_plot$myvar)
df_plot$myvar = gsub(df_plot$myvar, pattern = '_', replacement = '-')

# Plot all or separately for vowels and consonants
for (plotType in c('all', 'vwl', 'cons')) {
  if (plotType == 'all') {
    df_plot1 = df_plot
  } else if (plotType == 'vwl') {
    df_plot1 = droplevels(df_plot[df_plot$myvar %in% c('backness', 'height', 'roundedness', 'extreme', 'extreme-roundedness'), ])
  } else if (plotType == 'cons') {
    df_plot1 = droplevels(df_plot[df_plot$myvar %in% c('voicing', 'manner', 'manner-voicing', 'position', 'position-voicing'), ])
  }
  
  df_plot1$myvar = factor(df_plot1$myvar, levels = soundClasses1[soundClasses1 %in% unique(df_plot1$myvar)])
  df_plot1$word_group = factor(df_plot1$word_group,
                               levels = rev(sort(unique(df_plot1$word_group))))
  hjust = ifelse(df_plot1$type == 'positive', 1, 0)
  breaks = c(-5:5, -threshold_log, +threshold_log) # sort(c(-5:5, threshold_log, -threshold_log))
  labels = c(paste0('1/', 2^(5:1)), 
             1,  # 0
             2^(1:5),
             2^(-threshold_log), 2^threshold_log)
  
  ggplot(df_plot1, aes(x = word_group, y = fit, ymin = lwr, ymax = upr, label = mylabel, color = outcome, fill = outcome, size = outcome)) +
    geom_hline(yintercept = threshold_log, linetype = 3) +
    geom_hline(yintercept = -threshold_log, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 2, color = 'gray70') +
    # annotate(geom = 'text', label = 'ROPE', x = -Inf, y = 0, vjust = 'bottom', size = 3) +
    geom_errorbar(size = .5, width = 0) +
    geom_point(shape = 21) +
    geom_text(aes(y = pos), nudge_x = 0.05, size = 2, hjust = hjust, vjust = 'bottom') +
    facet_wrap( ~  myvar, ncol = ifelse(plotType == 'all', 5, 3), scales = 'free_y') +
    ylab('Odds ratio (1 = chance, >1 = overrepresented, <1 = underrepresented)') +
    xlab('') +
    scale_x_discrete(expand = c(.05, .05)) +
    scale_y_continuous(breaks = breaks, labels = labels) +
    scale_color_manual(name = 'Type of outcome', values = c('black', 'gray40')) +
    scale_fill_manual(name = 'Type of outcome', values = c('black', 'gray40')) +
    scale_size_manual(name = 'Type of outcome', values = c(1.25, .75)) +
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position.inside = ifelse(plotType == 'all', 'none', c(.8, .2)))
  
  ggsave(filename = paste0('pix_repl2024/summary_', threshold, '_', plotType, '.png'), width = ifelse(plotType == 'all', 30, 20), height = ifelse(plotType == 'all', 21, 20), units = 'cm', dpi = 300, scale = 1)
}


df_niklas = df_plot[, c('outcome', 'myvar', 'type', 'group', 'word_caps', 'fit', 'lwr', 'upr', 'nLangs', 'nRegions', 'cardinal', 'topCardinal')]
df_niklas = df_niklas[order(df_niklas$outcome, df_niklas$myvar, df_niklas$type, df_niklas$group, df_niklas$word_caps), ]
# write.csv(df_niklas, paste0('data_derived/summary_', threshold, '.csv'), row.names = FALSE)


## All plots manually arranged in 2 rows
row_height = as.matrix(table(df_plot$fit > 0, df_plot$myvar))
row_height = apply(row_height, 2, max)  # max number of associations of each sign in each group (governs plot height)

plotme = function(groups) {
  temp = droplevels(df_plot[df_plot$myvar %in% groups, ])
  temp$myvar = factor(temp$myvar, levels = groups)
  temp$word_group = as.character(temp$word_group)
  temp$word_group = factor(temp$word_group, levels = rev(sort(unique(temp$word_group))))
  hjust = ifelse(temp$type == 'positive', 1, 0)
  breaks = c(-5:5, -threshold_log, threshold_log)
  labels = c(paste0('1/', 2^(5:1)), 
             1,  # 0
             2^(1:5),
             2^(-threshold_log), 2^threshold_log)
  
  ggplot(temp, aes(x = word_group, y = fit, ymin = lwr, ymax = upr, label = mylabel, color = outcome, fill = outcome, size = outcome)) +
    geom_hline(yintercept = threshold_log, linetype = 3) +
    geom_hline(yintercept = -threshold_log, linetype = 3) +
    geom_hline(yintercept = 0, linetype = 2, color = 'gray70') +
    # annotate(geom = 'text', label = 'ROPE', x = -Inf, y = 0, vjust = 'bottom', size = 3) +
    geom_errorbar(size = .5, width = 0) +
    geom_point(shape = 21) +
    geom_text(aes(y = pos), nudge_x = 0.05, size = 3, hjust = hjust, vjust = 'bottom') +
    facet_wrap( ~  myvar, ncol = 2, scales = 'free_y') +
    ylab('Odds ratio (1 = chance, >1 = overrepresented, <1 = underrepresented)') +
    xlab('') +
    scale_x_discrete(expand = c(.05, .05)) +
    scale_y_continuous(breaks = breaks, labels = labels, limits = c(-1, 1.25)) +
    scale_color_manual(name = 'Type of outcome', values = c('black', 'gray40')) +
    scale_fill_manual(name = 'Type of outcome', values = c('black', 'gray40')) +
    scale_size_manual(name = 'Type of outcome', values = c(1.25, .75)) +
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = 'none')
  
  filename = paste0('pix_repl2024/two_columns/', paste0(groups, collapse = '-'), '.png')
  ggsave(filename = filename, width = 20, height = max(row_height[groups]) / 3 + 4, units = 'cm', dpi = 300, scale = 1.2)
}

sort(row_height)
# plotme(c('position', 'position-voicing'))
# plotme(c('manner', 'manner-voicing'))
plotme(c('height', 'extreme'))
plotme(c('backness', 'extreme-roundedness'))
plotme(c('voicing', 'roundedness'))
