#---------------------------------#
##    Raw Goodness by k and p    ##
#---------------------------------#

library(ggplot2)

all.data <- df
all.data$k <- as.factor(all.data$k)

pdf('./Graphs/1-G_n800.pdf', height = 4, width = 4, paper = 'special')
ggplot(data = all.data, aes(x = p, y = G, color = k)) +
  geom_point() + xlab('Probability of Rewiring') +
  ylab('Goodness') + ggtitle('Goodness, n = 800')
dev.off()

