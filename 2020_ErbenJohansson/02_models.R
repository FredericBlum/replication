# Modeling: prepares a model-specific dataset, builds a dirichlet model, 
# saves fitted and observed values converted to log-odds,
# saves the associated cardinal sounds, and saves model-specific plots
library(brms)
library(ggplot2)
library(reshape2)
library(cmdstanr)
library(readr)
library(dplyr)

options(bitmapType="cairo")


#############################
### CONTROL PARAMETERS
#############################
# What levels are we modeling?
# 2: voicing, roundedness
# 3: height, backness
# 4: extreme
# 5: position
# 8: extreme_roundedness
# 10: position_voicing

myvar <- 'voicing'
grType <- c('cardinal', 'gr35', 'gr60')[1]
drop_rare_levels <- FALSE  # drop levels with very few observations (for manner_voicing, unvoiced laterals, vibrants, nasals; for position_voicing, remove voiced glottals)

# brms settings
folder_model <- 'models'  # path to folder for saving .rds files

# Where and how to save stuff
folder_data <- 'data'  # path to folder with original .csv files
folder_data_derived <- 'data_derived'  # path to folder with derived .csv files
folder_fig <- 'pix_repl2024'

# Plotting options
threshold=log2(1.20)  # for plot_fitted: 25% increased or decreased odds of finding the sound

#############################
### END OF CONTROL PARAMETERS
#############################

# Load the main datasets and helper functions

# Make sure all output folders exist
for (f in c(folder_data, folder_data_derived, folder_fig)) {
  if (!dir.exists(f)) dir.create(f)
}

odds=function(x) {
  # x[x >= 1]=.99  # cap odds at 100/1, otherwise bad for plotting
  return(x / (1 - x))
}

countBy=function(groupingVar, normBy, dataSource, simplex_in_one_column) {
  # groupingVar=myvar, normBy=mv, dataSource=df1, simplex_in_one_column=TRUE
  # set up the new dataframe
  out <- data.frame(lang_word=unique(dataSource$lang_word), language=NA, word=NA, stringsAsFactors=FALSE)
  groupingVar_levels <- unique(dataSource[, groupingVar])
  out[, normBy] <- dataSource[match(out$lang_word, dataSource$lang_word), normBy]
  out[, groupingVar_levels] <- NA

  # calculate phoneme class counts per word and lang
  myt <- unclass(table(dataSource[, groupingVar], dataSource$word, dataSource$language))

  # for each word in data, transform counts to proportions
  time_start=proc.time()
  nr=nrow(out)
  for (i in 1:nr) {
    out[i, c('language', 'word')]=unlist(strsplit(out$lang_word[i], 'ж'))
    temp=myt[, out$word[i], out$language[i]]
    temp1=temp / out[i, normBy]  # proportion of the total number of vowels OR consonants OR phonemes in this word
    temp2=temp1 / 1.002 + 0.001  # make sure there are no exact zeros or ones
    m=which.max(temp2)
    temp2[m]=1 - sum(temp2[-m])  # make sure they sum exactly to 1 by (very slightly) adjusting the largest %
    out[i, groupingVar_levels]=temp2
  }

  if (simplex_in_one_column) {
    # reformat the response simplex into a single column, so brm can read it properly
    respDir=as.matrix(out[, groupingVar_levels])
    colnames(respDir)=1:ncol(respDir)  # otherwise brm crashes if these vars contain '-', '_', etc.
    out$respDir=respDir
    out=out[, c('language', 'word', 'respDir')]
  }
  return(out)
}

# load data
df <- read.csv(paste0(folder_data, '/langs_all_longFormat.csv'), stringsAsFactors = TRUE)
ipa <- read_csv(paste0(folder_data, '/phonetic_groups.csv'))
ipa$vowelConsonant <- ifelse(ipa$height != '', 'vowel')

# add phonetic info to the dataset
includeVars <- c('height', 'backness', 'roundedness', 'extreme', 'extreme_roundedness', 'manner', 'manner_voicing', 'position', 'position_voicing', 'voicing', 'vowelConsonant', 'gr35', 'gr60', 'cardinal')
df[, includeVars] <- ipa[match(df$unicode, ipa$unicode), includeVars]


# words that are uncommonly long/short
avg_length <- df %>% group_by(word) %>% summarize(mean=mean(nPhonemesPerWord))
avg_length %>% arrange(mean)
avg_length %>% arrange(-mean)

# Coverage
unique(df[c("word", "language")]) %>% group_by(word) %>% count() %>% arrange(n)


# Subset data: for vowel features, focus only on vowels; etc.
if (myvar %in% c('manner', 'manner_voicing', 'position', 'position_voicing', 'voicing')) {
  mySounds <- 'consonants'
  df1 <- df %>% filter(is.na(vowelConsonant))
  mv='nConsPerWord'
} else if (myvar %in% c('height', 'backness', 'roundedness', 'extreme', 'extreme_roundedness')) {
  mySounds='vowels'
  df1 <- df %>% filter(vowelConsonant == 'vowel')
  mv='nVowelsPerWord'
}

if (drop_rare_levels) {
  if (myvar == 'manner_voicing') {
    df1=droplevels(df1[!df1$manner_voicing %in% c('lateral-voice', 'nasal-voice', 'vibrant-voice'), ])
  } else if (myvar == 'position_voicing') {
    df1=droplevels(df1[!df1$position_voicing %in% c('glottal+voice'), ])
  } else if (myvar == 'extreme_roundedness') {
    df1=droplevels(df1[!df1$extreme_roundedness %in% c('low-front-rounded', 'high-front-rounded'), ])
  }
}

df1$lang_word <- paste0(df1$language, 'ж', df1$word)  # 'ж' because '_' etc can be found in values
myPropVars <- unique(df1[, myvar])
n_levels <- length(myPropVars)

# Reformat data into a format suitable for brm - save on disk to save a few min
data_file=paste0(folder_data_derived, '/dirichlet_', myvar, '.rds')
if (file.exists(data_file)) {
  data=readRDS(data_file)
} else {
  data=countBy(groupingVar=myvar, normBy=mv, dataSource=df1, simplex_in_one_column=TRUE)
  saveRDS(data, data_file)
}
head(data)


# Add info about coordinates
langs <- read.csv('languoid.csv') %>% select(iso639P3code, latitude, longitude, id)

lang_info <- df1 %>% select(language, iso, region) %>% unique()

model_data <- data %>% left_join(lang_info) %>%
  left_join(langs, by=join_by(iso==iso639P3code)) 
model_data$word

## Model
mod_name=paste0(folder_model, '/repl2024_', myvar, '.rds')

mod <- brm(
  data = model_data,
  family = 'dirichlet',
  formula = respDir ~ 1 + (1|word) + (1|language) + gp(longitude, latitude, gr=TRUE),
  prior=c(
    # Log-odds prior
    prior(gamma(1, 1), class=phi),
    
    # Intercept for each category
    prior(normal(0, 0.5), class=Intercept, dpar = 'mu2'),
    # prior(normal(0, 0.5), class=Intercept, dpar = 'mu3'),
    # prior(normal(0, 0.5), class=Intercept, dpar = 'mu4'),
    # prior(normal(0, 0.5), class=Intercept, dpar = 'mu5'),
    # prior(normal(0, 0.5), class=Intercept, dpar = 'mu6'),
    # prior(normal(0, 0.5), class=Intercept, dpar = 'mu7'),
    # prior(normal(0, 0.5), class=Intercept, dpar = 'mu8'),
    #prior(normal(0, 0.5), class=Intercept, dpar = 'mu9'),
    #prior(normal(0, 0.5), class=Intercept, dpar = 'mu10'),
    
    # Standard deviations of intercepts
    prior(gamma(3, 30), class=sd, dpar='mu2'),
    # prior(gamma(3, 30), class=sd, dpar='mu3'),
    # prior(gamma(3, 30), class=sd, dpar='mu4'),
    # prior(gamma(3, 30), class=sd, dpar='mu5'),
    # prior(gamma(3, 30), class=sd, dpar='mu6'),
    # prior(gamma(3, 30), class=sd, dpar='mu7'),
    # prior(gamma(3, 30), class=sd, dpar='mu8'),
    #prior(gamma(3, 30), class=sd, dpar='mu9'),
    #prior(gamma(3, 30), class=sd, dpar='mu10'),
    
    # Standard deviations of GP
    prior(gamma(3, 30), class=sdgp, dpar='mu2'),
    # prior(gamma(3, 30), class=sdgp, dpar='mu3'),
    # prior(gamma(3, 30), class=sdgp, dpar='mu4'),
    # prior(gamma(3, 30), class=sdgp, dpar='mu5'),
    # prior(gamma(3, 30), class=sdgp, dpar='mu6'),
    # prior(gamma(3, 30), class=sdgp, dpar='mu7'),
    # prior(gamma(3, 30), class=sdgp, dpar='mu8')#,
    #prior(gamma(3, 30), class=sdgp, dpar='mu9'),
    #prior(gamma(3, 30), class=sdgp, dpar='mu10')
    ),
  silent=0,
  backend='cmdstanr',
  control=list(adapt_delta=0.90, max_treedepth=10),
  file=mod_name,
  threads=threading(18),
  iter=5000, warmup=2500, chains=4, cores=4
  )

new_data <- tibble(word=unique(model_data$word), latitude=0, longitude=150)

fit_name=paste0(folder_data_derived, '/repl2024_fit_', myvar, '.rds')
if (file.exists(fit_name)) {
  fit <- readRDS(file=fit_name)
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  fit=fitted(mod, newdata=new_data, re_formula='~(1|word)', summary=FALSE)
  saveRDS(fit, file=fit_name)  
}

# dim(fit)  # rows=MCMC, columns=words, dim3=levels of myvar
# Save fitted proportions per word

fit_propName=paste0(folder_data_derived, '/repl2024_fitProp_', myvar, '.rds')
if (file.exists(fit_propName)) {
  fit_prop <- readRDS(file=fit_propName)
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  fit_prop=fitted(mod, newdata=new_data, re_formula='~(1|word)', summary=TRUE, robust=TRUE)
  saveRDS(fit_prop, file=fit_propName)
}

rownames(fit_prop)=unique(model_data$word)
colnames(fit_prop)=c('fit', 'se', 'lwr', 'upr')
dimnames(fit_prop)[[3]]=myPropVars
fit_prop_df=NULL
for (k in 1:length(myPropVars)) {
  temp=as.data.frame(fit_prop[, , k])
  temp$word=rownames(temp)
  temp$group=myPropVars[k]
  if (is.null(fit_prop_df)) {
    fit_prop_df=temp
  } else {
    fit_prop_df=rbind(fit_prop_df, temp)
  }
}

# # Transform % into log-odds ratios
for (i in 1:nrow(fit)) {  # for each iteration in MCMC
  for (j in 1:n_levels) {  # for each level of myvar (eg 'rounded' / 'unrounded')
    # log-odds ratio: % in each word (vector of length n_words) / % in all words (one number)
    fit[i, , j]=log2(odds(fit[i, , j]) / odds(mean(fit[i, , j])))  # or fit[i, , j]=fit[i, , j] - mean(fit[i, , j])
  }
}

# Set up a dataframe for storing fitted values (log-odds ratios per word and per level of myvar)
df_plot=expand.grid(word=unique(data$word), group=myPropVars)
df_plot[, c('fit', 'lwr', 'upr')]=NA
for (w in 1:ncol(fit)) {
  word=df_plot$word[w]
  for (j in 1:n_levels) {
    group=myPropVars[j]
    row_idx=which(df_plot$word == word & df_plot$group == group)
    df_plot[row_idx, c('fit', 'lwr', 'upr')]=quantile(fit[, w, j], probs=c(.5, .025, .975))
  }
}
df_plot$word_caps=as.factor(toupper(df_plot$word))
head(df_plot)

# How many languages & regions contain the phoneme(s) in question?
for (i in 1:nrow(df_plot)) {
  idx=which(df1$word == as.character(df_plot$word[i]) & df1[, myvar] == as.character(df_plot$group[i]))
  df_plot$nLangs[i]=length(unique(df1$language[idx]))
  df_plot$nRegions[i]=length(unique(df1$region[idx]))
}

## OBSERVED FREQUENCIES
# Reformat the simplex column into a normal df
temp=data[, c(1, 2)]
temp[, myPropVars]=data[, 3]
# head(temp)

# Average proportions across languages
df_obs=data.frame(word=levels(df1$word))
for (i in 1:nrow(df_obs)) {
  temp2=temp[temp$word == df_obs$word[i], myPropVars]
  df_obs[i, myPropVars]=colMeans(temp2)
}
# rowSums(df_obs[, 2:ncol(df_obs)])  # all add up to 1, so no worries :))
# head(df_obs)

# Observed log-odds ratios
df_obs_OR=df_obs
a_obs=colMeans(df_obs[, 2:ncol(df_obs)])
a_obs_odds=odds(a_obs)
for (i in 1:nrow(df_obs_OR)) {
  df_obs_OR[i, 2:ncol(df_obs_OR)]=log2(odds(df_obs[i, 2:ncol(df_obs)]) / odds(a_obs))
}
df_plot_obs_OR=reshape2::melt(df_obs_OR, id='word')
# head(df_obs_OR)
# colMeans(df_obs_OR[, 2:ncol(df_obs_OR)])
df_plot_obs=reshape2::melt(df_obs, id='word')


## OBSERVED FREQUENCIES PER CARDINAL
# Count proportions of cardinal sounds in each word: pretty slow (~6 min),
# so it pays to save on disk each of three types of the tables
# of % cardinal sounds (consonants / vowels / both)
card_file=paste0(folder_data_derived, '/', grType, '_', mySounds, '.csv')
if (file.exists(card_file)) {
  card_table=read.csv(card_file)
} else {
  df_card=countBy(groupingVar=grType, normBy=mv, dataSource=df1,
                    simplex_in_one_column=FALSE)
  # head(df_card)
  card_levels=levels(df1[, grType])

  # Average proportions across languages
  card_table=data.frame(word=levels(df1$word))
  for (i in 1:nrow(card_table)) {
    temp=df_card[df_card$word == card_table$word[i], card_levels]
    card_table[i, card_levels]=colMeans(temp)
  }
  write.csv(card_table, card_file, row.names=FALSE)
}
# head(card_table)

# Reformat and add acoustic class (myvar) for each cardinal sound
dft_gr=reshape2::melt(card_table, id='word')
colnames(dft_gr)=c('word', grType, 'prop')
dft_gr[, myvar]=ipa[match(dft_gr[, grType], ipa[, grType]), myvar]
dft_gr$odds=odds(dft_gr$prop)
# head(dft_gr)

# Convert to log-odds ratios
a_gr=aggregate(prop ~ get(grType), dft_gr, mean)  # sum(a_gr$prop) == 1
a_gr$odds=odds(a_gr$prop)
colnames(a_gr)[1]=grType
# head(a_gr)
dft_gr$logOdds=log2(dft_gr$odds / a_gr$odds[match(dft_gr[, grType], a_gr[, grType])])  # log-odds ratio
# head(dft_gr)

# Find over-/under-represented cardinal sounds and add these labels to df_plot (fitted values)
df_plot$topCardinal=df_plot$cardinal=''
for (i in 1:nrow(df_plot)) {
  if (!is.na(df_plot$fit[i]) && df_plot$fit[i] > 0) {
    # positive (overrepresented)
    temp=dft_gr[dft_gr$word == df_plot$word[i] &
                    as.character(dft_gr[, myvar]) == as.character(df_plot$group[i]) &
                    dft_gr$logOdds > 0, ]
    if (nrow(temp) > 0) {
      temp=temp[order(temp$logOdds, decreasing=TRUE), ]
      df_plot$cardinal[i]=paste0(temp$cardinal, collapse='')
      df_plot$topCardinal[i]=as.character(temp$cardinal[which.max(temp$prop)])
    }
  } else {
    # negative (underrepresented
    temp=dft_gr[dft_gr$word == df_plot$word[i] &
                    as.character(dft_gr[, myvar]) == as.character(df_plot$group[i]) &
                    dft_gr$logOdds < 0, ]
    if (nrow(temp) > 0) {
      temp=temp[order(temp$logOdds, decreasing=FALSE), ]
      df_plot$cardinal[i]=paste0(temp$cardinal, collapse='')
      df_plot$topCardinal[i]=as.character(temp$cardinal[which.min(temp$prop)])
    }
  }
}
# df_plot[1:10, c('word', 'cardinal', 'topCardinal')]
# mean(df_plot$cardinal == '')  # in some rare cases (~1%), the fit and observed log-odds have different signs, ie they are very close to 0, but on different sides - basically noise

# Combine fitted and observed proportions
df_plot[, c('fitProp_fit', 'fitProp_lwr', 'fitProp_upr')]=fit_prop_df[, c('fit', 'lwr', 'upr')]
df_plot$obs=df_plot_obs$value
df_plot$obs_OR=df_plot_obs_OR$value
# colMeans(df_obs[, 2:ncol(df_obs)])  # mean % per group
# or: apply(df_obs[, 2:ncol(df_obs)], 2, median)
# cf: aggregate(fitProp_fit ~ group, df_plot, mean)

# Plot fitted values
df_plot_copy=df_plot
df_plot_copy$dim=(df_plot_copy$lwr > threshold | df_plot_copy$upr < -threshold)
df_plot_copy$word_caps=factor(df_plot_copy$word_caps, levels=rev(levels(df_plot_copy$word_caps)))
df_plot_copy$word_dim=ifelse(df_plot_copy$dim, as.character(df_plot_copy$word_caps), '')

fit_plot1 <- ggplot(df_plot_copy, aes(x=word_caps, y=fitProp_fit, ymin=fitProp_lwr, ymax=fitProp_upr, color=dim, label=word_dim)) +
  geom_point() +
  geom_errorbar(width=0) +
  geom_text(size=3, nudge_x=3) +
  geom_point(aes(x=word_caps, y=obs), inherit.aes=FALSE, shape=4) +
  scale_color_manual(values=c(rgb(0, 0, 0, alpha=.1, maxColorValue=1), rgb(0, 0, 0, maxColorValue=1))) +
  scale_x_discrete(labels=NULL, expand=c(0.02, 0.02)) +
  scale_y_continuous(limits=c(0, 1)) +
  xlab('Concept') +
  ylab('Proportion, %') +
  coord_flip() +
  facet_wrap(~group, ncol=n_levels) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position='none')
ggsave(filename=paste0(folder_fig, '/fit_1_', myvar, '.png'), fit_plot1)

fit_plot2 <- ggplot(df_plot_copy, aes(x=word_caps, y=fit, ymin=lwr, ymax=upr, color=dim, label=word_dim)) +
  geom_point(aes(x=word_caps, y=obs_OR), inherit.aes=FALSE, shape=4, size=.75) +
  geom_point() +
  geom_errorbar(width=0) +
  geom_text(size=3, nudge_x=3) +
  scale_color_manual(values=c(rgb(0, 0, 0, alpha=.1, maxColorValue=1), rgb(0, 0, 0, maxColorValue=1))) +
  scale_x_discrete(labels=NULL, expand=c(0.02, 0.02)) +
  # scale_y_continuous(breaks=-5:5, labels=c(paste0('1/', 2^(5:1)), 1, 2^(1:5))) +
  xlab('Concept') +
  ylab('Log-odds ratio') +
  geom_hline(yintercept=0, linetype=3) +
  geom_hline(yintercept=threshold, linetype=2) +
  geom_hline(yintercept=-threshold, linetype=2) +
  coord_flip() +
  facet_wrap(~group, ncol=n_levels) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position='none')

ggsave(filename=paste0(folder_fig, '/fit_2_', myvar, '.png'), fit_plot2)


write.csv(df_plot, paste0(folder_data_derived, '/', myvar, '.csv'))
