# Direct vs Gibbs sampling for bivariate normal distribution

# Initial setting
mu1 <- 1
mu2 <- 1
rho <- 0.9
mu <- c(mu1,mu2)
Sigma = matrix(c(1,rho,rho,1),2,2)
nDraws <- 500 # Number of draws

library(MASS) # To access the mvrnorm() function

# Direct sampling from bivariate normal distribution
directDraws <- mvrnorm(nDraws, mu, Sigma)

# Gibbs sampling
gibbsDraws <- matrix(0,nDraws,2)
theta2 <- 0 # Initial value for theta2
for (i in 1:nDraws){
  
  # Update theta1 given theta2
  theta1 <- rnorm(1, mean = mu1 + rho*(theta2-mu2), sd = sqrt(1-rho**2))
  gibbsDraws[i,1] <- theta1
  
  # Update theta2 given theta1
  theta2 <- rnorm(1, mean = mu2 + rho*(theta1-mu1), sd = sqrt(1-rho**2))
  gibbsDraws[i,2] <- theta2
  
}

a_Direct <- acf(directDraws[,1])
a_Gibbs <- acf(gibbsDraws[,1])

IF_Gibbs <- 1+2*sum(a_Gibbs$acf[-1])

par(mfrow=c(2,4))

# DIRECT SAMPLING
plot(1:nDraws, directDraws[,1], type = "l",col="blue") # traceplot of direct draws

hist(directDraws[,1],col="blue") # histogram of direct draws

cusumData <- cumsum(directDraws[,1])/seq(1,nDraws) # Cumulative mean value of theta1, direct draws
plot(1:nDraws, cusumData, type = "l", col="blue")

barplot(height = a_Direct$acf[-1],col="blue") # acf for direct draws

# GIBBS SAMPLING
plot(1:nDraws, gibbsDraws[,1], type = "l",col="red") # traceplot of Gibbs draws

hist(gibbsDraws[,1],col="red") # histogram of Gibbs draws

cusumData =  cumsum(gibbsDraws[,1])/seq(1,nDraws) # Cumulative mean value of theta1, Gibbs draws
plot(1:nDraws, cusumData, type = "l", col="red")

barplot(height = a_Gibbs$acf[-1],col="red") # acf for Gibbs draws

# Plotting the cumulative path of estimates of Pr(theta1>0, theta2>0)
par(mfrow=c(2,1))
plot(cumsum(directDraws[,1]>0 & directDraws[,2]>0)/seq(1,nDraws),type="l", main='Direct draws', xlab='Iteration number', ylab='', ylim = c(0,1))
plot(cumsum(gibbsDraws[,1]>0 & gibbsDraws[,2]>0)/seq(1,nDraws),type="l", main='Gibbs draws', xlab='Iteration number', ylab='', ylim = c(0,1))


