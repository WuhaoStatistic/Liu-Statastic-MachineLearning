# Question 1 Computations with Metropolis–Hastings
################################################################################
# Q1
################################################################################
pix <- function(x)
{
  return(x^5*exp(-x))
}
fMH_q1<-function(nstep,X0,props){
  vN<-1:nstep
  vX<-rep(X0,nstep);
  for (i in 2:nstep){
    X<-vX[i-1]
    Y<-rlnorm(1,meanlog=X,sdlog =props) 
    u<-runif(1)
    a<-min(c(1,(pix(Y)*dlnorm(X,meanlog=Y,sdlog=props))/(pix(X)*dlnorm(Y,meanlog=X,sdlog=props)))) 
    if (u <=a){vX[i]<-Y}else{vX[i]<-X}    
  }
  plot(vN,vX,pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",main="",ylim=c(min(X0-0.5,-5),max(5,X0+0.5)))
  return(vX)
}
t <- 1:10/5
q1_chains <- list() 
for(i in t)
  q1_chains[[i*5]] <- fMH_q1(10000,i,1)
rm(i,t)
# seems all the plots(start points from 0.2 to 2, gap = 0.2) are not converged.
################################################################################
# Q2
################################################################################
# n is degree of fredom in chi_square distribution
# using x - x%%1 as floor function 
fMH_q2<-function(nstep,X0){
  vN<-1:nstep
  vX<-rep(X0,nstep);
  for (i in 2:nstep){
    X<-vX[i-1]
    Y<-rchisq(1,df=floor(X+1))
    u<-runif(1)
    a<-min(c(1,(pix(Y)*dchisq(X,df=floor(Y+1)))/(pix(X)*dchisq(Y,df=floor(X+1))))) 
    if (u <=a){vX[i]<-Y}else{vX[i]<-X}    
  }
  plot(vN,vX,pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",main="",ylim=c(min(X0-0.5,-5),max(5,X0+0.5)))
  return(vX)
}
t <- 1:10/5
q2_chains <- list() 
for(i in t)
  q2_chains[[i*5]] <- fMH_q2(10000,i)
rm(i,t)
# when Xt < 1, the plot will converge, when x > 1,plot will not converge.
################################################################################
# Q3
################################################################################
library(coda)
mc_list <- mcmc.list()
for (i in 1:10) 
{
  fmh_vX <- fMH_q2(300,i)
  mc_list[[i]] <- as.mcmc(fmh_vX)
}
gelman.diag(mc_list)
# the upper limit is 1.11(less than 1.2).so the chains are converged
################################################################################
# Q4
################################################################################
# the sample from step 1 and 2 are stored in q1_chains and q2_chains
# using the theory from course slides
# f(x) = g(x) * p(x);
# let p(x) be the target density function
# Since our generated sample is in the form of target density function,
# so X ~ p(x) and integral p(x) = 1
# so what we need to compute is actually the estimation of g(x)
# so what we need to do is using mean() function to get sample estimation.
mean_1 <- c()
mean_2 <- c()
for (i in 1:10) 
{
  mean_1 <- c(mean_1,mean(q1_chains[[i]]))  
  mean_2 <- c(mean_2,mean(q2_chains[[i]]))  
}
################################################################################
# Q5
################################################################################
# the estimate for gamma distribution(6,1) is 6.
# previous results are mean_1 and mean_2, we can see they are really closed to 6.
rm(list=ls())
################################################################################
################################################################################
load('chemical.RData')
library(ggplot2)
data2 <- data.frame(X=X,Y=Y,mu=colMeans(sa))
damelt <- reshape2::melt(data2,id='X')
ggplot(damelt,aes(x=X,y=value,color=variable))+
  geom_line()+
  geom_point()

data_gibbs <- data.frame(n=1:1000,mu_n=sa[,50])
ggplot(data_gibbs,aes(x=n,y=mu_n))+
geom_line()





