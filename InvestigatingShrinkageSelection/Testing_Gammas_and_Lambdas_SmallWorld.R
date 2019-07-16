#-------------------------------------#
##    Attempting to get a non-SW:    ##
##      Using Lambda and Gamma       ##
#-------------------------------------#

## Architecture of the Loop----------------------------------------------------
# Nested For Loops
# For loop for gamma, same as before
# Nested loop: lambda loop from 0 to max lambda
# Output: Dataframe of gamma, lambda, sw index


## Creating the For Loop-------------------------------------------------------
# Load in Data and Small World Function
library (qgraph)
library (IsingFit)
library (igraph)
library (glmnet)
library(ggplot2)

source('Dalege_Small_World_Function.R')
source('OrrIsingFit_FINAL.R')

load(file = 'Reagan1984.Rdata')

gammas <- seq(0,1,0.1)

gammas.22lambdas <- data.frame()

for (g in gammas) {
  gammaIsing <- IsingFit(na.omit(Reagan1984), gamma = g,
                         lowerbound.lambda = 0, progressbar = FALSE)
  
  lams <- gammaIsing$lambda.values
  
  temp.graph <- graph.adjacency(gammaIsing $ weiadj, 
                                 "undirected", diag = FALSE, 
                                 weighted = TRUE)
  
  gammasw <- SW_Index(temp.graph)
  
  row <- c(g, gammasw$SW.Index, lams)
  gammas.22lambdas <- rbind(gammas.22lambdas, row)
  
  print(g)
}

nodes <- paste('N', seq(1,22), sep = '')
names(gammas.22lambdas) <- c('gamma', 'swIndex', nodes)
write.csv(gammas.22lambdas, file = 'Gamma_List22lambdas.csv')


lambdas <- seq(0, 0.05, by = 0.005)
sw.gammas.lambdas <- data.frame()

setwd('./OrrIsingModels/')

for(g in gammas){
  for(y in lambdas){
    real.ising <- Orr.IsingFit(na.omit(Reagan1984), gamma = g,
                           lowerbound.lambda = 0, progressbar = TRUE,
                           glm.lam = y, family = 'binomial', AND = TRUE,
                           plot = FALSE)
    temp.igraph <- graph.adjacency(real.ising $ weiadj, 
                                   "undirected", diag = FALSE, 
                                   weighted = TRUE)
    sw <- SW_Index(temp.igraph)$SW.Index[1]
    
    cl <- ifelse(sw > SW_Index(temp.igraph)$Upper.CI[1,2],
                 ifelse(sw > SW_Index(temp.igraph)$Upper.CI[2,2],
                        ifelse(sw > SW_Index(temp.igraph)$Upper.CI[3,2],
                               ifelse(sw > SW_Index(temp.igraph)$Upper.CI[4,2],
                                      SW_Index(temp.igraph)$Upper.CI[4,1],
                                      SW_Index(temp.igraph)$Upper.CI[3,1]),
                               SW_Index(temp.igraph)$Upper.CI[2,1]),
                        SW_Index(temp.igraph)$Upper.CI[1,1]),
                 1)
    
    
    row <- c(g, y, sw, cl)
    sw.gammas.lambdas <- rbind(sw.gammas.lambdas, row)
    print(row)
    
    wgts <- as.data.frame(real.ising$weiadj)
    wgts$n1 <- rownames(wgts)
    wgtslong <- melt(wgts, id.vars = "n1")
    names(wgtslong) <- c('n1', 'n2', 'weight')
    
    write.csv(wgtslong, 
              file = paste('Orr_Ising_gamma_', g, '_lambda_', y, '.csv', sep = ''),
              row.names = FALSE)
    
  }
}

names(sw.gammas.lambdas) <- c("gamma", "lambda", "Swi", "ConfLevel")
setwd('..')
write.csv(sw.gammas.lambdas, file = 'Gamma_Lambda_SmallWorldIndex.csv')


sw.gammas.lambdas$ConfLabel <- NA
sw.gammas.lambdas[which(sw.gammas.lambdas$ConfLevel == 1),5] <- ' '
sw.gammas.lambdas[which(sw.gammas.lambdas$ConfLevel == 0.1),5] <- '*'
sw.gammas.lambdas[which(sw.gammas.lambdas$ConfLevel == 0.05),5] <- '**'
sw.gammas.lambdas[which(sw.gammas.lambdas$ConfLevel == 0.01),5] <- '***'
sw.gammas.lambdas[which(sw.gammas.lambdas$ConfLevel == 0.001),5] <- '****'


png(filename = "SW_gamma_lambda.png", width = 1200, height = 700)
  ggplot(sw.gammas.lambdas, aes(x = gamma, y = Swi, 
                                color = lambda, label = ConfLabel)) +
    geom_point(size = 3) + geom_text(nudge_y = 0.005, size = 10) +
    ggtitle("Small World Index based on Shrinkage and Selection Parameters") +
    xlab("Gamma (used in EBIC selection)") + ylab("Small World Index") +
    scale_color_continuous(name = "Lambda\n(used in shrinkage)") +
    theme(text = element_text(size = 18))
dev.off()


## * = CL of 0.10
## ** = CL of 0.05
## *** = CL of 0.01
## **** = CL of 0.001
