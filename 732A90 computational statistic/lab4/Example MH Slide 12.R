f.MCMC.MH<-function(nstep,X0,props){
    vN<-1:nstep
    vX<-rep(X0,nstep);
    for (i in 2:nstep){
	X<-vX[i-1]
	Y<-rnorm(1,mean=X,sd=props) 
	u<-runif(1)
	a<-min(c(1,(dnorm(Y)*dnorm(X,mean=Y,sd=props))/(dnorm(X)*dnorm(Y,mean=X,sd=props)))) 
	if (u <=a){vX[i]<-Y}else{vX[i]<-X}    
    }
    plot(vN,vX,pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",main="",ylim=c(min(X0-0.5,-5),max(5,X0+0.5)))
    abline(h=0)
    abline(h=1.96)
    abline(h=-1.96)
}

f.MCMC.MH(500, 0, 0.5)
f.MCMC.MH(500, 0, 0.1)
f.MCMC.MH(500, 0, 20)