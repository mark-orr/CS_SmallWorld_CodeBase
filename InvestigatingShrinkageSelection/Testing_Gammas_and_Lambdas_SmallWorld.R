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

source('Dalege Small World Function.R')

load(file = 'Reagan1984.Rdata')

gammas <- seq(0,1,0.1)

sw.gammas.lambdas <- data.frame()

for(g in gammas){
  g = 0
  temp.ising <- IsingFit(na.omit(Reagan1984), gamma = g, plot = FALSE, progressbar = FALSE)
  lambdas <- seq(0, max(temp.ising$lambda.values), 0.01)
  for(y in lambdas){
    real.ising <- IsingFit(na.omit(Reagan1984), gamma = g,
                           lowerbound.lambda = y, progressbar = FALSE)
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
  }
}

names(sw.gammas.lambdas) <- c("gamma", "lambda", "Swi", "ConfLevel")
write.csv(sw.gammas.lambdas, file = 'Gamma_Lambda_SmallWorldIndex.csv')
