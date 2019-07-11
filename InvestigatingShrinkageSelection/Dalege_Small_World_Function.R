#-------------------------------------#
##   Dalege's Small World Function   ##
#-------------------------------------#

SW_Index <- function (Graph, ci = c (.1, .05, .01, .001))
{
  randomC <- vector (, 1000)
  randomL <- vector (, 1000)
  for (i in 1:1000)
  {
    Rgraph <- erdos.renyi.game (vcount (Graph), ecount (Graph), 'gnm')
    randomC [i] <- transitivity (Rgraph, 'average')
    randomL [i] <- average.path.length(Rgraph)
  }
  MrandomC <- mean (randomC)
  MrandomL <- mean (randomL)
  Clustering.Graph = transitivity (Graph, 'average')
  ASPL.Graph = average.path.length (Graph)
  Index <- (Clustering.Graph / MrandomC) / (ASPL.Graph / MrandomL)
  
  sm_sample <- vector (, 1000)
  for (i in 1:1000)
  {
    Rgraph <- erdos.renyi.game (vcount (Graph), ecount (Graph), 'gnm')
    sm_sample [i] <- (transitivity (Rgraph, 'average') / MrandomC) /(average.path.length(Rgraph) / MrandomL)
  }
  CI <- as.vector (((quantile (sm_sample, 1 - (ci / 2)) - quantile (sm_sample, ci / 2)) / 2) + 1)
  return (list (SW.Index = Index, Upper.CI = data.frame (CI = ci, Value.CI = CI), 
                Clustering.Graph = Clustering.Graph, Clustering.Random.Graph = MrandomC,
                ASPL.Graph = ASPL.Graph, ASPL.Random.Graph = MrandomL))
}