#SWGraph_To_Lens_Weights_FastGraph.r
rm(list=ls())

#ENV VARS
env.vars <- Sys.getenv(c("n","p","k","r"))

library(igraph)
library(qgraph)

#DEFINE GRAPH
n <- as.numeric(env.vars[1])
p.rewire <- as.numeric(env.vars[2])
k <- as.numeric(env.vars[3])
k.imp <- k/2
#EDGE VALUE SDs
e.pos.sd <- 0.10
e.neg.sd <- 0.10

#NEW WATTS EMILY CODE STARTS HERE
g.0 <- watts.strogatz.game(dim = 1, size = n, 
                           nei = k.imp, p = 0)
g.1 <-  watts.strogatz.game(dim = 1, size = n, 
                           nei = k.imp, p = p.rewire)

#MIG START HERE
g.0.dist <- distances(g.0)
g.1.dist <- distances(g.1)

delta <- g.1.dist - g.0.dist
delta[delta < 0] <- -1
delta[delta >= 0] <-  1

weights <- as.matrix(as_adj(g.1)) * delta
ngraph <- graph_from_adjacency_matrix(weights, weighted = TRUE,
                                      mode = c('undirected'))
lens.weights <- get.data.frame(ngraph)

#ORR CODE
wt.app.fun <- function(x){
    ifelse(x==1,
       return(rnorm(1,mean=1,sd=e.pos.sd)),
       return(rnorm(1,mean=-1,sd=e.neg.sd)))   
}
lens.weights$noise.wt <- apply(data.frame(lens.weights$weight),1,wt.app.fun)

#FINAL MIGRATION POINT
lens.weights.use <- lens.weights[,c(1,2,4)]

#EMILY CODE ENDS HERE WITH THIS ADDITION
use.list <- lens.weights.use

#WRITE FOR FURTHER ANALYSIS
write.table(use.list, 
            file=paste("../Weight_Generation/Weight_Files/EdgeList_",
                "n",env.vars[1],".",
                "p",env.vars[2],".",
                "k",env.vars[3],".",
                "r",env.vars[4],".",
                "wt",sep=""),
            row.names=FALSE, col.names=FALSE)

#use.list IS THE EQUIVALENT TO WHAT IS NEEDED FROM EMILY NEW CODE--A DIRECTED GRAPH THAT NEEDS TO BE TURNED INTO BIDIRECTIONAL BELOW

#ALIGH WITH EM CODE WITH THIS
final.list.for.import <- use.list
#FROM EMILY 01
# Bidirectional to unidirectional
mark.i <- final.list.for.import[,1]
mark.j <- final.list.for.import[,2]
mark.w <- final.list.for.import[,3]

# Original weights by Mark
mark.o <- cbind(mark.i, mark.j)
mark.o <- cbind(mark.o, mark.w)

# Reversed weights by Mark
mark.r <- cbind(mark.j, mark.i)
mark.r <- cbind(mark.r, mark.w)

# Complete Weights
gen.weights <- rbind(mark.o, mark.r)

#NOW EXPORT TO LENS WITH MORE OF EMILY CODE
#------------------------#
##   Create Edgelist    ##
#------------------------#

# setwd(" ")
# Source your code: ClusteredGraphAndPolarized...

## Create Unpopulated Edgelist --------------------------------------
## with bias unit
node.i <- rep(seq(1:n),n+1)
node.j <- rep(seq(0:n),n)
sw.weights <- cbind(node.i, node.j)

sw.weights[which(sw.weights[,2] == n+1),2] <- 0
sw.weights <- sw.weights[order(sw.weights[,1], sw.weights[,2]),]
sw.weights <- as.data.frame(sw.weights)
names(sw.weights) <- c('node.i', 'node.j')


## Merge SW generated weights with dataframe ------------------------
gen.weights <- as.data.frame(gen.weights)
names(gen.weights) <- c('node.i', 'node.j', 'weight')

sw.weights.merged <- merge(sw.weights, gen.weights,
                           by = c('node.i', 'node.j'),
                           all.x = TRUE)
sw.weights.merged <- sw.weights.merged[order(
                        sw.weights.merged[,1],
                        sw.weights.merged[,2]),]

sw.weights.merged[is.na(sw.weights.merged)] <- 0


## Exporting to Lens ------------------------------------------------
lens.props <- c(1431655766, n*n+n, 1, 0)
#OLD lens.props <- c(1431655766, 1000000, 1, 0)
lens.props <- as.data.frame(lens.props, ncol = 1)
names(lens.props) <- c("")
sw.weights.lens <- as.data.frame(sw.weights.merged[,3], ncol = 1)
names(sw.weights.lens) <- c("")
sw.weights.lens.final <- rbind(lens.props,sw.weights.lens)
write.table(sw.weights.lens.final, 
            file=paste("../Weight_Generation/Weight_Files/Set_",
                "n",env.vars[1],".",
                "p",env.vars[2],".",
                "k",env.vars[3],".",
                "r",env.vars[4],".",
                "wt",sep=""),
            row.names=FALSE, col.names=FALSE)

q(save="no")
#EOF
