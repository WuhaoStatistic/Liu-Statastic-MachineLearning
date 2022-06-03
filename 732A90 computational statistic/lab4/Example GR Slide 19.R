library(coda)
f1<-mcmc.list();f2<-mcmc.list();n<-100;k<-20
X1<-matrix(rnorm(n*k),ncol=k,nrow=n)
X2<-X1+(apply(X1,2,cumsum)*(matrix(rep(1:n,k),ncol=k)^2))
for (i in 1:k){f1[[i]]<-as.mcmc(X1[,i]);f2[[i]]<-as.mcmc(X2[,i])}
print(gelman.diag(f1))
# Potential scale reduction factors:
#     Point est. Upper C.I.
#[1,]      0.999       1.01

print(gelman.diag(f2))
# Potential scale reduction factors:
#     Point est. Upper C.I.
#[1,]       1.82       2.38
