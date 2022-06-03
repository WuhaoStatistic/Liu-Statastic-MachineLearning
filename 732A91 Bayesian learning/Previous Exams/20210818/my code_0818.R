# Q3
log_posterior <- function(theta,n,sum_log)
{
  log_pos = -n*theta**2+2*sum_log*theta
  return(log_pos)
}

theta = seq(-1,2,0.001)
log_pos = exp(log_posterior(theta,5,2))
log_pos = log_pos/sum(log_pos)/0.001
plot(theta,log_pos)


# Q1
#b
y <- c(2.32,1.82,2.4,2.08,2.13)
n <- length(y)
theta <- rgamma(1e4,11,0.5+sum(y))
samples <- rgamma(1e4,2,rate = theta)
plot(density(samples))

# c
n_week = 30
n_draw = 1e4
history_res = matrix(nrow = n_draw,ncol=n_week)
for (i in 1:n_week) {
  thetas= rgamma(n_draw,11,0.5+sum(y))
  history_res[,i] = rgamma(n_draw,2,thetas)
}
mean(rowSums(history_res>2.4))

#d
loss <- function(alpha)
{
  x <- rep(0,n_draw)
  for (i in 1:n_draw) {
    x[i] = sum(history_res[i,] > 0.9 * log(alpha))
  }
  return(alpha+mean(x))
}

x1 <- c()
for (i in 1:1000) 
{
  x1 <- c(x1,loss(1+i/100))
}
1+which.min(x1)/100
