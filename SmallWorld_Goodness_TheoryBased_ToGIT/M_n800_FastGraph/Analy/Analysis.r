#TESTING USE OF SHELL ENV VAR PASSED TO LENS
#EACH S RECEIVED UNIQUE EXAMPLE SET

#m1c MALE
rm(list=ls())
#setwd("~/BehaveChange/R21Work/Analysis/m1d/")
d1 <- read.table("test_outputs.out",col.names=c("A","B"),fill=TRUE)
graph.name <- c("test_outputs")

length(d1$A)

#PARMS
num.units <- 100
num.epocs <- 1
num.examples <- 110
num.cycles <- 24 #THIS IS TO REFLECT THAT WE ARE CATCHING ONLY LAST CYCLE/EX
num.people <- 1
catch.rows.person <- num.epocs*num.examples*num.cycles #NUMBER OF CATCH ROWS PER
                                                       #PERSON
catch.rows <- catch.rows.person*num.people

#ADDED 10-27-11, the + 1 in num.units+1 TO CAPTURE THE COL FOR EXAMPLE NUMBER
#catch <- matrix(rep(NA,catch.rows*(num.units+1)),nrow=catch.rows,ncol=(num.units+1))
catch <- matrix(rep(NA,catch.rows*(num.units)),nrow=catch.rows,ncol=(num.units))

#ONE EXAMPLE IS THE SMALLEST ITERATIVE UNIT SO USE IT
#THE LENGHT OF AN EXAMPLE IS 1583 
#IF CHANGE ANYTHING BUT <num.epocs or num.people> WILL NEED TO CHANGE
#THE <x,y,z, etc> INDEXES IN LOOP BELOW
index.cyl.1 <- seq(1,catch.rows,24)
index.cyl.2 <- seq(2,catch.rows,24)
index.cyl.3 <- seq(3,catch.rows,24)
index.cyl.4 <- seq(4,catch.rows,24)
index.cyl.5 <- seq(5,catch.rows,24)
index.cyl.6 <- seq(6,catch.rows,24)
index.cyl.7 <- seq(7,catch.rows,24)
index.cyl.8 <- seq(8,catch.rows,24)
index.cyl.9 <- seq(9,catch.rows,24)
index.cyl.10 <- seq(10,catch.rows,24)
index.cyl.11 <- seq(11,catch.rows,24)
index.cyl.12 <- seq(12,catch.rows,24)
index.cyl.13 <- seq(13,catch.rows,24)
index.cyl.14 <- seq(14,catch.rows,24)
index.cyl.15 <- seq(15,catch.rows,24)
index.cyl.16 <- seq(16,catch.rows,24)
index.cyl.17 <- seq(17,catch.rows,24)
index.cyl.18 <- seq(18,catch.rows,24)
index.cyl.19 <- seq(19,catch.rows,24)
index.cyl.20 <- seq(20,catch.rows,24)
index.cyl.21 <- seq(21,catch.rows,24)
index.cyl.22 <- seq(22,catch.rows,24)
index.cyl.23 <- seq(23,catch.rows,24)
index.cyl.24 <- seq(24,catch.rows,24)


for (i in 1:(num.epocs*num.examples*num.people)) { 
  x <- d1$A[((i*2552)-2551):(i*2552)] #ONE EXAMPLE, 24 CYCLES
  a1 <- 107 #CONSTANTS FOR COMPUTING INDEX FOR CYCLES WITHIN EXAMPLE
  a2 <- 206
#  a3 <- 51
#  a4 <- 64
  c <- 102
  y.1 <- x[c(a1:a2)] #CYCLE NO INDEXED BY y.X
  y.2 <- x[c((a1+(c*1)):(a2+(c*1)))]
  y.3 <- x[c((a1+(c*2)):(a2+(c*2)))]
  y.4 <- x[c((a1+(c*3)):(a2+(c*3)))]
  y.5 <- x[c((a1+(c*4)):(a2+(c*4)))]
  y.6 <- x[c((a1+(c*5)):(a2+(c*5)))]
  y.7 <- x[c((a1+(c*6)):(a2+(c*6)))]
  y.8 <- x[c((a1+(c*7)):(a2+(c*7)))]
  y.9 <- x[c((a1+(c*8)):(a2+(c*8)))]
  y.10 <- x[c((a1+(c*9)):(a2+(c*9)))]
  y.11 <- x[c((a1+(c*10)):(a2+(c*10)))]
  y.12 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.13 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.14 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.15 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.16 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.17 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.18 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.19 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.20 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.21 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.22 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.23 <- x[c((a1+(c*11)):(a2+(c*11)))]
  y.24 <- x[c((a1+(c*11)):(a2+(c*11)))]
  
  #JUST CAPTURED ONE EXAMPLE-4 CYCLES

  #ADDED 10-27-11 CAPTURE THE EXAMPLE NUMBER
  #x.2 <- d1$B[((i*312)-311):(i*312)][1]
  
  #PUT IN CATCH
#  catch[index.cyl.1[i],] <- c(y.1,x.2)
#  catch[index.cyl.2[i],] <- c(y.2,x.2)
#  catch[index.cyl.3[i],] <- c(y.3,x.2)
#  catch[index.cyl.4[i],] <- c(y.4,x.2)
#  catch[index.cyl.5[i],] <- c(y.5,x.2)
#  catch[index.cyl.6[i],] <- c(y.6,x.2)
#  catch[index.cyl.7[i],] <- c(y.7,x.2)
#  catch[index.cyl.8[i],] <- c(y.8,x.2)
#  catch[index.cyl.9[i],] <- c(y.9,x.2)

  catch[index.cyl.1[i],] <- c(y.1)
  catch[index.cyl.2[i],] <- c(y.2)
  catch[index.cyl.3[i],] <- c(y.3)
  catch[index.cyl.4[i],] <- c(y.4)
  catch[index.cyl.5[i],] <- c(y.5)
  catch[index.cyl.6[i],] <- c(y.6)
  catch[index.cyl.7[i],] <- c(y.7)
  catch[index.cyl.8[i],] <- c(y.8)
  catch[index.cyl.9[i],] <- c(y.9)
  catch[index.cyl.10[i],] <- c(y.10)
  catch[index.cyl.11[i],] <- c(y.11)
  catch[index.cyl.12[i],] <- c(y.12)
  catch[index.cyl.13[i],] <- c(y.13)
  catch[index.cyl.14[i],] <- c(y.14)
  catch[index.cyl.15[i],] <- c(y.15)
  catch[index.cyl.16[i],] <- c(y.16)
  catch[index.cyl.17[i],] <- c(y.17)
  catch[index.cyl.18[i],] <- c(y.18)
  catch[index.cyl.19[i],] <- c(y.19)
  catch[index.cyl.20[i],] <- c(y.20)
  catch[index.cyl.21[i],] <- c(y.21)
  catch[index.cyl.22[i],] <- c(y.22)
  catch[index.cyl.23[i],] <- c(y.23)
  catch[index.cyl.24[i],] <- c(y.24)
  
}

#CATCH AS DATAFRAME
#catch <- data.frame(catch)
#MAKE PERSON INDEX IN CATCHxs
#catch$person.i <- rep(1:num.people,rep(catch.rows.person,num.people))

#NEW PLOTS FOR CAN MODEL
pdf(paste(graph.name,"_Raw.pdf",sep=""))
par(mfrow=c(10,10))
par(mar=c(.2,.2,.2,.2))
par(oma=c(5,5,5,0))
for(j in seq(1,2640,24) ){ #2400 REFLECTS 24*100 (num exampels by ticks)
    plot(catch[j:(j+23),1],ylim=c(0,1),type="b")
    for(i in 2:100){ #UNITS IN NNET
        #ifelse((i==16|i==18|i==20|i==22),a <- 2,a <- 1)
        #ifelse((i==16|i==18|i==20|i==22),b <- 3,b <- 1)
        points(catch[j:(j+23),i],ylim=c(0,1),type="b",col=1,pch=1)
    }
}
dev.off()

#ATTRACTOR SIMILARITY 
#NEED LAST--24th--line from each block in the graph above.
#SHOULD GET no examples as no lines
catch.attractor <- catch[seq(24,2640,24),1:100] #100 is number of nodes in nnet
att.mat <- catch.attractor
library(lsa)
#COSINE GIVES SIM MATRIX FOR COL VECTORS
#SO USE TRANSPOSE
att.mat.t <- t(att.mat)
att.mat.sim <- cosine(att.mat.t)

pdf(paste(graph.name,"_SimilarityHist.pdf",sep=""))
hist(att.mat.sim,breaks=300,xlim=c(0,1))
dev.off()



#HEATMAP APPROACH TAKEN FROM somewhere/Analysis_CAN_FullParam.r
#NEW STUFF HAVE NOT MODIFIED THE CODE YET TO FIT THE NEW SIMS...JUST COPIED IT.

#HEAT MAP APPROACH

#TEST ONE OFF
library(gplots)
j <- 1
#for(j in seq(1,catch.ex.by.cyc,24) ){ 

#FIRST ONE IS POSITIVE
a <- catch[j:(j+23),1]
#THE REST OF THE POSITIVES
for(i in c(2:15,seq(17,21,2))){ #UNITS IN NNET
    a <- rbind(a,catch[j:(j+23),i])
}
#THE NEGATIVES
for(i in seq(16,22,2)){ #UNITS IN NNET
    a <- rbind(a,catch[j:(j+23),i])
}

row.names(a) <- c(1:15,seq(17,21,2),seq(16,22,2))

heatmap.2(a,scale="none",key=TRUE,keysize=1,density.info="none",dendrogram=c("none"),trace="none",Rowv=FALSE,Colv=FALSE,symkey=FALSE,col=my_palette,labRow=row.names(a),labCol=FALSE)

#NOW LOOP IT
#THIS WORKS BUT THE PAR MFROW DOES NOT...
pdf(paste(graph.name,"_HeatMap.pdf",sep=""))
par(mfrow=c(6,6))
par(mar=c(.2,.2,.2,.2))
par(oma=c(5,5,5,0))
my_palette <- colorRampPalette(c("black","grey","white"))(n = 299)
a <- 1:10
for(j in seq(1,catch.ex.by.cyc,24) ){
    
    rm(a)
    #FIRST ONE IS POSITIVE
    a <- catch[j:(j+23),1]
    #THE REST OF THE POSITIVES
    for(i in c(2:15,seq(17,21,2))){ #UNITS IN NNET
        a <- rbind(a,catch[j:(j+23),i])
    }
    #THE NEGATIVES
    for(i in seq(16,22,2)){ #UNITS IN NNET
        a <- rbind(a,catch[j:(j+23),i])
    }

    row.names(a) <- c(1:15,seq(17,21,2),seq(16,22,2))

    heatmap.2(a,scale="none",key=TRUE,keysize=1,density.info="none",dendrogram=c("none"),trace="none",Rowv=FALSE,Colv=FALSE,symkey=FALSE,col=my_palette,labRow=row.names(a),labCol=FALSE)
    
}

dev.off()


#LOOP IT BUT MAKE A SEPARATE GRAPH FOR EACH ONE
my_palette <- colorRampPalette(c("black","grey","white"))(n = 299)

sim.name <- 0
for(j in seq(1,catch.ex.by.cyc,24) ){
    sim.name <- sim.name+1
    #FIRST ONE IS POSITIVE
    a <- catch[j:(j+23),1]
    #THE REST OF THE POSITIVES
    for(i in c(2:15,seq(17,21,2))){ #UNITS IN NNET
        a <- rbind(a,catch[j:(j+23),i])
    }
    #THE NEGATIVES
    for(i in seq(16,22,2)){ #UNITS IN NNET
        a <- rbind(a,catch[j:(j+23),i])
    }
    
    row.names(a) <- c(1:15,seq(17,21,2),seq(16,22,2))
    
    pdf(paste(graph.name,"SimNo_",sim.name,"_HeatMap.pdf",sep=""))
    par(mar=c(.2,.2,.2,.2))
    par(oma=c(5,5,5,0))
    
    heatmap.2(a,scale="none",key=TRUE,keysize=1,density.info="none",dendrogram=c("none"),trace="none",Rowv=FALSE,Colv=FALSE,symkey=FALSE,col=my_palette,labRow=row.names(a),labCol=FALSE)
    dev.off()    
}


#MAKE A FINAL STATE GRAPH COMPARING ALL 33 SIMS
for(j in seq(1,catch.ex.by.cyc,24) ){ #J is sim name

    #FIRST ONE IS POSITIVE
    a <- catch[j:(j+23),1]
    #THE REST OF THE POSITIVES
    for(i in c(2:15,seq(17,21,2))){ #UNITS IN NNET
        a <- rbind(a,catch[j:(j+23),i])
    }
    #THE NEGATIVES
    for(i in seq(16,22,2)){ #UNITS IN NNET
        a <- rbind(a,catch[j:(j+23),i])
    }

    ifelse(j==1,b <- a[,24], b <- cbind(b,a[,24]))
}


row.names(b) <- c(1:15,seq(17,21,2),seq(16,22,2))
names(b) <- 1:33

pdf(paste(graph.name,"AllSimsEndState_HeatMap.pdf",sep=""))
    
heatmap.2(b,scale="none",key=TRUE,keysize=1,density.info="none",dendrogram=c("none"),trace="none",Rowv=FALSE,Colv=FALSE,symkey=FALSE,col=my_palette,labRow=row.names(b),labCol=names(b))

dev.off()    




#END HEATMAP STUFF






#EOF
