#####################################################################################
# Generates samples from the joint posterior distribution of the parameters
# in the x1,....xn iid Normal(theta,sigma^2) model with
# prior p(theta,sigma^2) propto 1/sigma^2
#####################################################################################
NormalNonInfoPrior <- function(NDraws,Data){
  Datamean <- mean(Data)
  s2 <- var(Data)
  n <- length(Data)
  PostDraws <- matrix(0,NDraws,2)
  PostDraws[,2] <- ((n-1)*s2)/rchisq(NDraws,n-1)
  PostDraws[,1] <- rnorm(NDraws,mean=Datamean,sd=sqrt(PostDraws[,2]/n))
  
  return(PostDraws)
}

Nobs <- 10000
Ndraws <- 10000
Data <- rnorm(Nobs,5,10) 	# Sampling Nobs observations from the N(5,10) density##
PostDraws <- NormalNonInfoPrior(Ndraws,Data) # Generating draws from the joint posterior of mu and sigma^2
hist(PostDraws[,1]) 			# Plotting the histogram of mu-draws
hist(PostDraws[,2])       # Plotting the histogram of sigma^2-draws

# Examples of probability calculations
mean(PostDraws[,1]>4.9 & PostDraws[,1]<5.1) # Approximate posterior probability of 4.9 < mu < 5.1
mean(PostDraws[,2]>99 & PostDraws[,2]<101)  # Approximate posterior probability of 99 < sigma^2 < 101


########################################################################################
# Generate samples from the joint posterior distribution of theta=(theta_1,...,theta_K)
# for the multinomial model with K categories and a Dirichlet prior for theta.
########################################################################################
Dirichlet <- function(NDraws,y,alpha){
  K <- length(alpha)
  xDraws <- matrix(0,NDraws,K)
  thetaDraws <- matrix(0,NDraws,K) # Matrix where the posterior draws of theta are stored
  for (j in 1:K){
    xDraws[,j] <- rgamma(NDraws,shape=alpha[j]+y[j],rate=1)
  }
  for (ii in 1:NDraws){
    thetaDraws[ii,] <- xDraws[ii,]/sum(xDraws[ii,])
  }
  return(thetaDraws)
}

###########   Setting up data and prior  #################
y <- c(180,230,62,41) # Data of counts for each category
p <- y/sum(y)
alpha_const <- 1
alpha <- alpha_const*c(15,15,10,10) # Dirichlet prior hyperparameters
NDraws <- 10000 # Number of posterior draws

###########   Posterior sampling from Dirichlet  #################
thetaDraws <- Dirichlet(NDraws,y,alpha)

K <- length(y)
########### Summary statistics from the posterior sample #########
for (k in 1:K){
  mean(thetaDraws[,k])
  sqrt(var(thetaDraws[,k]))
}

sum(thetaDraws[,2]>thetaDraws[,1])/NDraws # p(theta2>theta1 | y)
# Posterior probability that Android has largest share, i.e. p(theta_2 > max(theta_1,theta_3,theta_4) | y)
Index_max <- matrix(0,NDraws,1)
for (ii in 1:NDraws){
Index_max[ii,1] <- which.max(thetaDraws[ii,])
}
mean(Index_max==2)

# Plot histograms of the posterior draws
plot.new() # Opens a new graphical window
par(mfrow = c(2,2)) # Splits the graphical window in four parts (2-by-2 structure)
hist(thetaDraws[,1],25) # Plots the histogram of theta[,1] in the upper left subgraph
hist(thetaDraws[,2],25)
hist(thetaDraws[,3],25)
hist(thetaDraws[,4],25)


