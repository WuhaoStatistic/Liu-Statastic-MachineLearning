floglik<-function(y, mu, sigma2, n){ -0.5*n*log(2*pi*sigma2)-0.5*sum((y-mu)^2)/sigma2}

EM.Norm<-function(Y,eps,kmax){
    Yobs <- Y[!is.na(Y)];Ymiss <- Y[is.na(Y)]
    n <- length(c(Yobs, Ymiss));r <- length(Yobs)

    k<-1;muk<-1;sigma2k<-0.1

    llvalprev<-floglik(Yobs,muk,sigma2k,r);
    llvalcurr<-llvalprev+10+100*eps
    print(c(muk,sigma2k,llvalcurr))

    while ((abs(llvalprev-llvalcurr)>eps) && (k<(kmax+1))){
	llvalprev<-llvalcurr
	## E-step
	EY<-sum(Yobs)+(n-r)*muk
	EY2<-sum(Yobs^2)+(n-r)*(muk^2+sigma2k)

	## M-step
	muk<-EY/n
	sigma2k<-EY2/n-muk^2

	## Compute log-likelihood
	llvalcurr<-floglik(Yobs,muk,sigma2k,r)
	k<-k+1

	print(c(muk,sigma2k,llvalcurr))
    }
}
