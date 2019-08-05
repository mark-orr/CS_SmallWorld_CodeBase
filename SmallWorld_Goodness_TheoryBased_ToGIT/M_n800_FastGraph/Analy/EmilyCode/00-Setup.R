#-----------------------------#
##    Set Up for Analysis    ##
#-----------------------------#

# setwd()

rm(list = ls())
setwd("./")

## Read in the data ---------------------------------------
raw  <- read.table('goodnessn800.out')



## Specify k values ---------------------------------------
KK <- c(13, 91, 130, 169, 208, 247, 286, 325, 364, 403)


## Specify the p-rewire  values ---------------------------
PP <- c(0, 0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.50, 0.75, 1.00)


## Specify the number of rewirings ------------------------
II <- seq(1:10)


