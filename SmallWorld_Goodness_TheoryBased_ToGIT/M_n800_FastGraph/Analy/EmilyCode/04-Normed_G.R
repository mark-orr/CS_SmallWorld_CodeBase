#---------------------------------#
##  Normed Goodness by k and p   ##
#---------------------------------#

library(ggplot2)

all.data <- df


normed.data <- data.frame()
for(k in KK){
    sub.n <- all.data[which(all.data$k == k),]
    sub.n$Gnorm <- (sub.n$G - mean(sub.n$G))/sd(sub.n$G)
    normed.data <- rbind(normed.data, sub.n)
}

normed.data$k <- as.factor(normed.data$k)

pdf('./Graphs/2-G_norm_n800.pdf', heigh = 4, width = 4, paper = 'special')
ggplot(data = normed.data, aes(x = p, y = Gnorm, color = k)) +
  geom_point() + xlab('Probability of Rewiring') +
  ylab('Goodness Normalized') + 
  ggtitle('Goodness Normalized Across k, n = 800')
dev.off()

