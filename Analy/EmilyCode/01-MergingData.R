#-----------------------------#
## Creating Master Dataframe ##
#-----------------------------#

## Creating the labels ------------------------------------
df <- data.frame()
for(k in KK){
  for(p in PP){
    for(i in II){
      row <- c(k, p, i)
      df  <- rbind(df, row)
    }
  }
}
df <- cbind(df, raw)

names(df) <- c('k', 'p', 'i', 'G')


