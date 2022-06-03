### Select Logistic or Probit regression and install packages ###
Probit <- 0
# install.packages("mvtnorm") # package that contains the multivariate normal pdf
library("mvtnorm") # reads the mvtnorm package into R's memory. We can now use the necessary function dmvnorm.

### Prior and data inputs ###
Covs <- c(1:11) # Select which covariates/features to include
standardize <- TRUE # If TRUE, covariates/features are standardized to mean 0 and variance 1
lambda <- 1# scaling factor for the prior of beta 

# Loading wine quality data set (https://archive.ics.uci.edu/ml/datasets/Wine+Quality)
WineData <- data.frame(readRDS("WineData.rds")) # read data from file
Nobs <- dim(WineData)[1] # number of observations
y <- matrix(0,Nobs,1) # y=1 if the quality of wine is above 5, otherwise y=0.
for (ii in 1:Nobs){
  if (WineData$quality[ii] > 5){
    y[ii] <- 1
  }
}
WineData <- data.frame(intercept=rep(1,Nobs),WineData) # add intercept
X <- as.matrix(WineData[,Covs]);
Xnames <- colnames(X)
if (standardize){
  Index <- 2:(length(Covs)-1)
  X[,Index] <- scale(X[,Index])
}
Npar <- dim(X)[2]

# Setting up the prior
mu <- as.matrix(rep(0,Npar)) # Prior mean vector
Sigma <- (1/lambda)*diag(Npar) # Prior covariance matrix

# Functions that returns the log posterior for the logistic and probit regression.
# First input argument of this function must be the parameters we optimize on, 
# i.e. the regression coefficients beta.

LogPostLogistic <- function(betas,y,X,mu,Sigma){
  linPred <- X%*%betas;
  logLik <- sum( linPred*y - log(1 + exp(linPred)) );
  #if (abs(logLik) == Inf) logLik = -20000; # Likelihood is not finite, stear the optimizer away from here!
  logPrior <- dmvnorm(betas, mu, Sigma, log=TRUE);
  
  return(logLik + logPrior)
}

LogPostProbit <- function(betas,y,X,mu,Sigma){
  linPred <- X%*%betas;
  SmallVal <- .Machine$double.xmin
  logLik <- sum(y*log(pnorm(linPred)+SmallVal) + (1-y)*log(1-pnorm(linPred)+SmallVal) )
  logPrior <- dmvnorm(betas, mu, Sigma, log=TRUE);
  return(logLik + logPrior)
}

# Select the initial values for beta
initVal <- matrix(0,Npar,1)

if (Probit==1){
  logPost = LogPostProbit;
} else{
  logPost = LogPostLogistic;
}

# The argument control is a list of options to the optimizer optim, where fnscale=-1 means that we minimize 
# the negative log posterior. Hence, we maximize the log posterior.  
OptimRes <- optim(initVal,logPost,gr=NULL,y,X,mu,Sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

# Printing the results to the screen
names(OptimRes$par) <- Xnames # Naming the coefficient by covariates
approxPostStd <- sqrt(diag(-solve(OptimRes$hessian))) # Computing approximate standard deviations.
names(approxPostStd) <- Xnames # Naming the coefficient by covariates
print('The posterior mode is:')
print(OptimRes$par)
print('The approximate posterior standard deviation is:')
approxPostStd <- sqrt(diag(-solve(OptimRes$hessian)))
print(approxPostStd)
