# Bayesian Learning Exam 2021-10-21
# Run this file once during the exam to get all the required data
# and functions for the exam in working memory
# Author: Bertil Wegmann

###############################
########## Problem 1 ########## 
###############################

###############################
########## Problem 2 ########## 
###############################

# Reading the data from file
load(file = 'Disease.RData')
library(mvtnorm)

BayesLogitReg <- function(y, X, mu_0, Sigma_0, nIter){
  
  # Sampling from a logistic regression with prior:
  #
  # beta ~ N(mu_0, Sigma_0)
  # 
  # INPUTS:
  #   y - n-by-1 vector with response data observations
  #   X - n-by-nCovs matrix with covariates, first column should be ones if you want an intercept.
  #   mu_0 - prior mean for beta
  #   Sigma_0  - prior covariance matrix for beta
  #   nIter - Number of samples from the posterior (iterations)
  #
  # OUTPUTS:
  #   results$betaSample     - Posterior sample of beta.     nIter-by-nCovs matrix
  
  LogPostLogistic <- function(betas,y,X,mu_0,Sigma_0){
    linPred <- X%*%betas;
    logLik <- sum( linPred*y - log(1 + exp(linPred)) );
    if (abs(logLik) == Inf){
      logLik = -20000; # Likelihood is not finite, stear the optimizer away from here!
    }
    logPrior <- dmvnorm(betas, mu_0, Sigma_0, log=TRUE);
    
    return(logLik + logPrior)
  }
  
  Npar <- dim(X)[2]
  initVal <- matrix(0,Npar,1)
  
  OptimRes <- optim(initVal,LogPostLogistic,gr=NULL,y,X,mu_0,Sigma_0,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)
  
  mu_n <- OptimRes$par
  Sigma_n <- -solve(OptimRes$hessian)
  betaSample <- rmvnorm(nIter,mu_n,Sigma_n)
  
  return(results = list(betaSample=betaSample))
}

###############################
########## Problem 3 ########## 
###############################


