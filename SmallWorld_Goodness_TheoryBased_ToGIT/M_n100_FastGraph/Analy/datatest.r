rm(list=ls())

#PARAMS
i <- 3 #number of runs per param point
exs <- 110 #number of examples per run
params <- 101 #number of param points
r <- i*exs*params #total runs across params in lens

#FOR TESTING GEN DATA
#d <- data.frame(seq(1:r))
#names(d)[1] <- "V1"

#REAL DATA
d <- read.table("goodness.out")

pdf("test_noise_fastGraph.pdf")
plot(d$V1)
dev.off()
#ACTUAL P USED IN PLOT
d$p <- rep(seq(0,1,.01),times=1,each=i*exs)

#FOR TAPPLY TO GET MEANS
d$meta.index <- as.factor(rep(1:params,times=1,each=i*exs))

#SIMPLE G over P plot
pdf("AveGoodnessOverP.pdf")
plot(tapply(d$p,d$meta.index,mean),tapply(d$V1,d$meta.index,mean),main="Goodness Over P Rewire, N=100, k=4",xlab="p",ylab="Goodness")
abline(v=c(.01,.20),col=2,lty=2)
dev.off()

#CAN SEE THAT MIN GOODNESS IS FOR SPARSE INPUTS AND SMALL WORLD
pdf("AllData.pdf")
plot(d$V1)
dev.off()

#LOOK AT SEGMENTS
#USE THIS TO PINPOINT
seq(1,30000,330)
plot(d$V1[1:330])

#GRAB MIN AND MAX
pdf("MinGoodnessOverP.pdf")
plot(tapply(d$p,d$meta.index,mean),tapply(d$V1,d$meta.index,min),main="Goodness Over P Rewire, N=100, k=4",xlab="p",ylab="Goodness")
abline(v=c(.01,.20),col=2,lty=2)
dev.off()

pdf("MaxGoodnessOverP.pdf")
plot(tapply(d$p,d$meta.index,mean),tapply(d$V1,d$meta.index,max),main="Goodness Over P Rewire, N=100, k=4",xlab="p",ylab="Goodness")
abline(v=c(.01,.20),col=2,lty=2)
dev.off()


#EOF
