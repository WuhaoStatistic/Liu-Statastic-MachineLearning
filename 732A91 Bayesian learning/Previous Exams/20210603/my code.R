#2
log_postertio <- function(theta,n,sum_square)
{
  return(n*log(theta)-theta*(0.5+sum_square))
}

theta = seq(0.1,10,0.01)
theta_res = exp(log_postertio(theta,13,2.8))
theta_res = theta_res/sum(theta_res)/0.01
plot(theta,theta_res)

theta_hat = 3.9

opt = optim(theta_hat,log_postertio,n=13,sum_square=2.8,lower = 0.1,method = c('L-BFGS-B'),hessian=TRUE,control=list(fnscale=-1))
theta_hat = opt$par
hessian = opt$hessian[1,1]
lines1 = dnorm(seq(0.1,10,0.01),theta_hat,-1/hessian)
plot(theta,theta_res)
lines(theta,lines1)


#1
n = 100
alpha1 = 16
beta1 = 24
s = 38
f = 100-s
alpha2 = alpha1+s
beta2 = beta1+f
pbeta(0.4,alpha2,beta2,lower.tail = FALSE)

#b
n_sample = 3500
sample = rbeta(n_sample,alpha2,beta2)
sample2 = 1-sample
library(ggplot2)
df = data.frame(n=1:n_sample,theta=sample2)
p3 = ggplot(df,aes(theta))+
  geom_histogram(aes(y=..density..),bins=40,color='purple',fill='white')+
  geom_density(alpha=0.3,color='red')+
  ggtitle('The density and distribution of 1- theta')
print(p3)

new_sample = sample2/sample
quantile(new_sample,prob=c(0.025,0.975))

#c
beta/beta



Dirichlet<-function(n_draws,y,theta)
{
  history_res = matrix(nrow = n_draws,ncol = length(theta))
  for (i in 1:length(theta)) {
    history_res[,i] = rgamma(n_draws,y[i]+theta[i])
  }
  for (i in 1:n_draws) {
    history_res[i,] = history_res[i,]/sum(history_res[i,])
  }
  return(mean(history_res[,1]>history_res[,3]))
}
Dirichlet(1e5,c(38,27,35),c(20,20,20))
