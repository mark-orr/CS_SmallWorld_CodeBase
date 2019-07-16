#-------------------------------------#
#        Pruning Delta Models         #
#-------------------------------------#

library(stringr)
library(tidyr)
library(igraph)
library(qgraph)
library(ggplot2)

## Loading in SW Function -----------------------------------------------------
source('Dalege_Small_World_Function.R')

## Loading in Weights ---------------------------------------------------------
dt <- read.table("DeltaEdgeList.txt", sep = " ")


## Stripping the .txt file of the unnessecary elements-------------------------
parsed_arrow <- sapply(X = dt$V1, FUN = str_split, '->')
p1 <- sapply(parsed_arrow, '[[', 1)
p2 <- sapply(parsed_arrow, '[[', 2)

dt$part1 <- p1
dt$part2 <- p2


## Formatting the R dataframe for graphical use--------------------------------
Input <- dt$part1
Output <- dt$part2
Weight <- dt$V2

df <- cbind(Input, Output, Weight)
df <- as.matrix(df)
net <- graph.edgelist(df[,1:2])
E(net)$weight <- as.numeric(df[,3])

df[,3] <- as.numeric(df[,3])


## Cleaning the Network--------------------------------------------------------
topcut <- max(abs(as.numeric(max(df[,3]))), as.numeric(min(df[,3])))

cuts <- seq(round(topcut, 1) - 0.1, 0, by = -0.1)
thresh.swi.cl <- data.frame()

setwd('./PrunedDeltaModels/')

for(threshold in cuts){ 
  
  clean_df <- df[which(df[,3] > threshold | 
                         df[,3] < -threshold),]
  clean_df <- as.matrix(clean_df)
  clean_net <- graph.edgelist(clean_df[,1:2])
  
  sw <- SW_Index(clean_net)$SW.Index
  
  cl <- ifelse(sw > SW_Index(clean_net)$Upper.CI[1,2],
               ifelse(sw > SW_Index(clean_net)$Upper.CI[2,2],
                      ifelse(sw > SW_Index(clean_net)$Upper.CI[3,2],
                             ifelse(sw > SW_Index(clean_net)$Upper.CI[4,2],
                                    SW_Index(clean_net)$Upper.CI[4,1],
                                    SW_Index(clean_net)$Upper.CI[3,1]),
                             SW_Index(clean_net)$Upper.CI[2,1]),
                      SW_Index(clean_net)$Upper.CI[1,1]),
               1)
  
  row <- c(threshold, sw, cl)
  thresh.swi.cl <- rbind(thresh.swi.cl, row)
  
  print(row)
  
  write.csv(clean_df, 
            file = paste('PrunedDelta_thresh_', threshold, '.csv', 
                         sep = ''), row.names = FALSE)
  
  
} # starts with most extreme pruning

setwd('..')
names(thresh.swi.cl) <- c("Threshold", "SmallWorldIndex", "ConfLevel")
write.csv(thresh.swi.cl, file = 'PrunedDelta_Threshold_Smallworld.csv',
          row.names = FALSE)

png(filename = "Pruned_Delta.png", width = 1200, height = 700)
  ggplot(thresh.swi.cl, aes(x = Threshold, y = SmallWorldIndex)) +
    geom_point() + geom_point(size = 3) + 
    ggtitle("Small World Index based on Pruning Threshold") +
    xlab("Threshold") + ylab("Small World Index") +
    theme(text = element_text(size = 18))
dev.off()
