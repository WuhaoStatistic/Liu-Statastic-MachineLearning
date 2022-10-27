library(bnlearn)
library(gRain)

#Z0 -> Z1 -> Z2
#|      |    |
#x0    x1    x2

net <- model2network("[Z0][X0|Z0][Z1|Z0][X1|Z1][Z2|Z1][X2|Z2]")
plot(net)
cptz0 <- matrix(c(0.5,0.5),ncol=2)
dimnames(cptz0) <- list(NULL,c('h','i'))

cptz1 <- matrix(c(0.9,0.1,0.2,0.8),nrow=2,ncol=2)
dim(cptz1) <- c(2,2)
dimnames(cptz1) <- list('Z1'=c('h','i'),'Z0'=c('h','i'))

cptz2 <- matrix(c(0.9,0.1,0.2,0.8),nrow=2,ncol=2)
dim(cptz2) <- c(2,2)
dimnames(cptz2) <- list('Z2'=c('h','i'),'Z1'=c('h','i'))

cptx0 <- matrix(c(0.6,0.4,0.3,0.7),nrow=2,ncol=2)
dim(cptx0) <- c(2,2)
dimnames(cptx0) <- list('X0'=c('h','i'),'Z0'=c('h','i'))

cptx1 <- matrix(c(0.6,0.4,0.3,0.7),nrow=2,ncol=2)
dim(cptx1) <- c(2,2)
dimnames(cptx1) <- list('X1'=c('h','i'),'Z1'=c('h','i'))

cptx2 <- matrix(c(0.6,0.4,0.3,0.7),nrow=2,ncol=2)
dim(cptx2) <- c(2,2)
dimnames(cptx2) <- list('X2'=c('h','i'),'Z2'=c('h','i'))

netfit <- custom.fit(net,list(Z0=cptz0, Z1=cptz1, Z2=cptz2, X0=cptx0, X1=cptx1, X2=cptx2))
netcom <- compile(as.grain(netfit))

#p(z0,z1,z2=h) = p(x0=h)*p(x1=h|x0=h)*p(x2=h|x1=h,x0=h)
querygrain(setEvidence(netcom,nodes=c("Z0"),states=c("h")),c("Z1"))
querygrain(setEvidence(netcom,nodes=c("Z1","Z0"),states=c("h","h")),c("Z2"))
print(0.5*0.9*0.9)

#2
querygrain(setEvidence(netcom,nodes=c("X1"),states=c("h")),c("Z0"))
querygrain(setEvidence(netcom,nodes=c("X1","Z0"),states=c("h","h")),c("Z1"))
querygrain(setEvidence(netcom,nodes=c("X1","Z0","Z1"),states=c("h","h","h")),c("Z2"))
print(0.6129032*0.9473684*0.9)
#Answer
# So the probability of that person is healthy in three day is 0.6129032*0.9473684*0.9 = 0.523
#3
querygrain(setEvidence(netcom,nodes=c("X1","X2"),states=c("h","h")),c("Z0"))[[1]][1]
querygrain(setEvidence(netcom,nodes=c("X1","Z0","X2"),states=c("h","h","h")),c("Z1"))[[1]][1]
querygrain(setEvidence(netcom,nodes=c("X1","Z0","Z1","X2"),states=c("h","h","h","h")),c("Z2"))[[1]][1]
print(0.6730038*0.9661017*0.9473684)
#Answer
# So the probability of that person is healthy in three day is 0.6730038*0.9661017*0.9473684 = 0.616

################################################################################
library(HMM)
library(entropy)
States=1:4 # rr, rs, sr ss
Symbols=1:2 #r s
##                  rr  rs   sr   ss
trans <- matrix(c(0.75,0.25,0.00,0.00,
                  0.00,0.00,0.50,0.50,
                  0.50,0.50,0.00,0.00,
                  0.00,0.00,0.25,0.75),nrow=4,ncol=4)
#               r    s
emi <- matrix(c(0.9,0.1,
                0.1,0.9,
                0.9,0.1,
                0.1,0.9),nrow=4,ncol=2)

startProbs=c(0.25,0.25,0.25,0.25)
hmm=initHMM(States,Symbols,startProbs,trans,emi)
simHMM(hmm,10)
################################################################################

################################################################################
Kernel4 <- function(x1,x2,sigmaF=1,l=0.5){
  n1 <- length(x1)
  n2 <- length(x2)
  K <- matrix(NA,n1,n2)
  for (i in 1:n2){
    K[,i] <- sigmaF^2*(1+sqrt(3)/l*sqrt(abs(x1-x2[i])))*exp(-sqrt(3)*abs(x1-x2[i])/l)
  }
  return(K)
}

zGrid = seq(0.01,1,by=0.01)
res = Kernel4(0,zGrid)
plot(seq(0.01,1,by=0.01),res,type='l')
res = Kernel4(0,zGrid,sigmaF=0.5,l=1)
plot(seq(0.01,1,by=0.01),res,type='l')
# Interpret the plot 
# With l=1, this plot is not so smooth, if we change to a larger l,it will be more
# smooth

# Repeat exercise
# Smaller sigmaF will scale down all kernel results, in this case, the variance of f will
# decrease.




