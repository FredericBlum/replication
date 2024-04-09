# Data preparation: converts the dataset in "full_dataset.csv" into the long format for modeling


df = read.csv('data/full_dataset.csv')  
ipa = read.csv('data/phonetic_groups.csv')
vowels = ipa$unicode[ipa$height != '']

out_temp = vector('list', length = (nrow(df) - 1) * (ncol(df) - 1))
counter = 1
for (i in 3:nrow(df)) {
  for (j in 2:ncol(df)) {
    temp = as.character(df[i, j])
    temp1 = strsplit(temp, "")[[1]]  # convert into a vector of characters
    if(length(temp1) > 0) {
      out_temp[[counter]] = data.frame(
        unicode = temp1,
        language = colnames(df)[j],
        iso = df[1, j],
        region = df[2, j],
        word = df$Language_name[i],
        sylIdx = 1:length(temp1),
        nPhonemesPerWord = nchar(temp),
        nVowelsPerWord = sum(temp1 %in% vowels)
      )
    }
    counter = counter + 1
  }  
  print(paste('Done with word', i - 1, 'of', nrow(df) - 1))
}
out = as.data.frame(data.table::rbindlist(out_temp))  # MUCH faster than just do.call("rbind", out)
out$nConsPerWord = out$nPhonemesPerWord - out$nVowelsPerWord
head(out)
# u = sort(as.character(unique(out$unicode)))  # make sure no suspicious symbols
# cat(u)
# # summary(out)
# length(unique(out$unicode))  # 182 unicode characters
# length(unique(out$word))     # 344 words

write.csv(out, 'data/langs_all_longFormat.csv')
