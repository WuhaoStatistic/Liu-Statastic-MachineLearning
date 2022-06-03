f.OU.MC<-function(nstep,X0){
    vN<-1:nstep
    vX<-rep(X0,nstep);X<-X0
    for (i in 2:nstep){
	X<-exp(-0.1)*X+rnorm(1,0,sqrt(2.5*(1-exp(-0.2))))
	vX[i]<-X
    }
    plot(vN,vX,pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",main="",ylim=c(min(X0-0.5,-5),max(5,X0+0.5)))
    abline(h=0)
    abline(h=1.96)
    abline(h=-1.96)
}

f.OU.MC(1000, -10)