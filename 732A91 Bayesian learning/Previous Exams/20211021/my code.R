#1 d

log_posterior <- function(theta,n,sum_x)
{
  pos = (2+sum_x)*log(theta)-(0.5+n)*theta
  return(pos)
}

theta = seq(2,9,0.001)
theta_sample = exp(log_posterior(theta,15,75))
theta_sample = theta_sample/sum(theta_sample)/0.001
plot(theta,theta_sample)

# 1 e
theta_hat = 5
opt = optim(theta_hat,log_posterior,n=15,sum_x=75,control=list(fnscale=-1),lower=3,method=c('L-BFGS-B'),hessian=TRUE)
theta_hat = opt$par
sigma = sqrt(-1/opt$hessian[1,1])
draws = dnorm(theta,theta_hat,sigma)

plot(theta,theta_sample)
lines(theta,draws,col='red')

# 1 f
history_res = matrix(nrow=1e4,ncol=1)
for (i in 1:1e4) {
  theta_1 = rgamma(1,3+75,rate = 15.5)
  history_res[i,] = max(rpois(15,theta_1))
}
mean(history_res[,1]>=14)

# 3b
sample = rnorm(5e5,92,sqrt(54))
plot(density(sample))

# 3c
uti <- function(mu,c)
{
  return(60+sqrt(c)*log(mu)-c)
}
n_draw = 5e4
history_res = matrix(nrow=n_draw,ncol=1)
for (i in 1:n_draw) {
  history_res[i,1] = rnorm(1,92,54)
}
mu = colMeans(history_res)[1]
c = seq(1,20,0.1)
plot(c,uti(mu,c))
lines(c,rep(max(uti(mu,c)),length(c)))
print(which.max(uti(mu,c))*0.1+1)
