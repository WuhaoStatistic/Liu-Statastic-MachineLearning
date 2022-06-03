f.MCMC.Gibbs<-function(nstep,X0,vmean,mVar){
    vN<-1:nstep
    d<-length(vmean)
    mX<-matrix(0,nrow=nstep,ncol=d)
    mX[1,]<-X0
    for (i in 2:nstep){
	X<-mX[i-1,]
	Y<-rep(0,d)
        Y[1]<-rnorm(1,mean=vmean[1]+(mVar[1,-1]%*%solve(mVar[-1,-1]))%*%(X[2:d]-vmean[-1]),sd=sqrt(mVar[1,1]-mVar[1,-1]%*%solve(mVar[-1,-1])%*%mVar[-1,1]))
	for (j in 2:(d-1)){
	    Y[j]<-rnorm(1,mean=vmean[j]+(mVar[j,-j]%*%solve(mVar[-j,-j]))%*%(c(Y[1:(j-1)],X[(j+1):d])-vmean[-j]),sd=sqrt(mVar[j,j]-mVar[j,-j]%*%solve(mVar[-j,-j])%*%mVar[-j,j]))
	}
	Y[d]<-rnorm(1,mean=vmean[d]+(mVar[d,-d]%*%solve(mVar[-d,-d]))%*%(Y[1:(d-1)]-vmean[-d]),sd=sqrt(mVar[d,d]-mVar[d,-d]%*%solve(mVar[-d,-d])%*%mVar[-d,d]))
	mX[i,]<-Y
    }
    mX
}

