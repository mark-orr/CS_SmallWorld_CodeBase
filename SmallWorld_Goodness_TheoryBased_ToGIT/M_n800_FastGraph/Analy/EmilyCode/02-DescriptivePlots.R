#----------------------------#
##    Descriptive Plots     ##
#----------------------------#
library(nortest)
library(ggplot2)

desc <- df

desc.stats <- data.frame()

for(k in KK){
  for(p in PP){
    sub <- desc[which(desc$k == k &
                        desc$p == p),]
    mean <- mean(sub$G)
    sd   <- sd(sub$G)
    med  <- median(sub$G)
    skew <- mean - med
    norm <- ad.test(sub$G)$p.value
    
    row  <- c(k, p, mean, sd, skew, norm)
    desc.stats <- rbind(desc.stats, row)
  }
}

names(desc.stats) <- c('k', 'p', 'mean', 'sd', 'skew', 'norm')
desc.stats$k <- as.factor(desc.stats$k)

## Visualizing the means ----------------------------------
pdf('./Graphs/3-meanG_by_p_k_n800.pdf', width = 4, height = 4, paper = 'special')
ggplot(data = desc.stats, aes(x = p, y = mean, 
                              color = k)) +
  geom_point() + scale_color_discrete() + geom_line() +
  ggtitle('Mean Goodness, n = 800') +
  xlab('Probability of re-wire') + ylab('Mean of Goodness')
dev.off()

## Visualizing the sds ----------------------------------
pdf('./Graphs/4-sdG_by_p_k_n800.pdf', width = 4, height = 4, paper = 'special')
ggplot(data = desc.stats, aes(x = p, y = sd, 
                              color = k)) +
  geom_point() + scale_color_discrete() + geom_line() +
  ggtitle('Std Dev of Goodness, n = 800') +
  xlab('Probability of re-wire') + ylab('Std Dev of Goodness')
dev.off()


## Visualizing the skews ----------------------------------
pdf('./Graphs/6-skewG_by_p_k_n800.pdf', width = 4, height = 4, paper = 'special')
ggplot(data = desc.stats, aes(x = p, y = skew, 
                              color = k)) +
  geom_point() + scale_color_discrete() + geom_line() +
  ggtitle('Skewness of Goodness, n = 800') +
  xlab('Probability of re-wire') + ylab('Skew of Goodness')
dev.off()


## Visualizing the Normality ------------------------------
pdf('./Graphs/5-normG_by_p_k_n800.pdf', width = 4, height = 4, paper = 'special')
ggplot(data = desc.stats, aes(x = p, y = norm, 
                              color = k)) +
  geom_point() + scale_color_discrete() + 
  geom_hline(aes(yintercept = 0.05), linetype = 'dashed') +
  ggtitle('Normality of Goodness, n = 800') +
  xlab('Probability of re-wire') + 
  ylab('Anderson-Darling Test p-value') + 
  annotate('text', x = 0.75, y = 0.08,
           label = 'Normality Cut Off')
dev.off()

