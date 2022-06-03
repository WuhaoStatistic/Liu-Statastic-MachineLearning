# f.MCMC.Gibbs in code for slide 15
vmean<-c(1,2)
mVar<-rbind(c(1,0.5),c(0.5,1))
nstep<-200
X0<-c(10,10)
mX<-f.MCMC.Gibbs(nstep,X0,vmean,mVar)

plot(mX[-1,1],mX[-1,2],pch=19,cex=0.5,col="black",xlab="X1",ylab="X2",main="",cex.lab=1.7,cex.axis=1.5,xlim=c(min(mX[-1,1]-0.5),max(mX[-1,1]+0.5)),ylim=c(min(mX[,2]-0.5),max(mX[-1,2]+0.5)))

par(mfrow=c(2,1))
plot(2:nstep,mX[-1,1],pch=19,cex=0.3,col="black",xlab="t",ylab="X1",cex.axis=1.5,cex.lab=1.7)
abline(h=vmean[1])
plot(2:nstep,mX[-1,2],pch=19,cex=0.3,col="black",xlab="t",ylab="X2",cex.axis=1.5,cex.lab=1.7)
abline(h=vmean[2])
