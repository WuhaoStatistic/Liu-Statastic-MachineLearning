############################################################################################
# MMs: EM algorithm for Gaussian MMs: For 732A99 only
############################################################################################

install.packages("mvtnorm")
library(mvtnorm)

set.seed(1234567890)

max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=300 # number of training points
D=2 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data

# Producing the training data
mu1<-c(0,0)
Sigma1 <- matrix(c(5,3,3,5),D,D)
dat1<-rmvnorm(n = 100, mu1, Sigma1)
mu2<-c(5,7)
Sigma2 <- matrix(c(5,-3,-3,5),D,D)
dat2<-rmvnorm(n = 100, mu2, Sigma2)
mu3<-c(8,3)
Sigma3 <- matrix(c(3,2,2,3),D,D)
dat3<-rmvnorm(n = 100, mu3, Sigma3)
plot(dat1,xlim=c(-10,15),ylim=c(-10,15))
points(dat2,col="red")
points(dat3,col="blue")
x[1:100,]<-dat1
x[101:200,]<-dat2
x[201:300,]<-dat3
plot(x,xlim=c(-10,15),ylim=c(-10,15))

K=3 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional means
Sigma <- array(dim=c(D,D,K)) # conditional covariances
llik <- vector(length = max_it) # log likelihood of the EM iterations

# Random initialization of the parameters
pi <- runif(K,0,1)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0,5)
  Sigma[,,k]<-c(1,0,0,1)
}
pi
mu
Sigma

for(it in 1:max_it) {
  # E-step: Computation of the fractional component assignments
  llik[it] <- 0
  for(n in 1:N) {
    for(k in 1:K) {
      z[n,k] <- pi[k]*dmvnorm(x[n,],mu[k,],Sigma[,,k])
    }
    
    #Log likelihood computation.
    llik[it] <- llik[it] + log(sum(z[n,]))
    
    z[n,] <- z[n,]/sum(z[n,])
  }
  
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()  
  # Stop if the lok likelihood has not changed significantly
  if (it > 1) {
    if(abs(llik[it] - llik[it-1]) < min_change) {
      break
    }
  }
  
  #M-step: ML parameter estimation from the data and fractional component assignments
  for(k in 1:K) {
    pi[k] <- sum(z[,k]) / N
    for(d in 1:D) {
      mu[k, d] <- sum(x[, d] * z[, k]) / sum(z[,k])
    }
    for(d in 1:D) {
      for(d2 in 1:D) {
        Sigma[d,d2,k]<-sum((x[, d]-mu[k,d]) * (x[, d2]-mu[k,d2]) * z[, k]) / sum(z[,k])
      }
    }
  }
}

plot(llik[1:it], type="l")

# The number of parameters in a MM model is the number of mixing coefficients minus 1 (because they all
# have to sum up to 1), plus the number of elements in the mean vector for each component, plus the number
# of entries in the covariance matrix for each component (since this matrix is symmetric, we only count
# the upper diagonal part of it). That is, K-1 mixing coefficients, plus K*D mean elements, plus K*D*(D+1)/2
# covariance elements.

BIC<-llik[it] - log(N) * 0.5 * ((K-1)+K*D+K*D*(D+1)/2)
BIC

# Producing the validation data
dat1<-rmvnorm(n = 1000, mu1, Sigma1)
dat2<-rmvnorm(n = 1000, mu2, Sigma2)
dat3<-rmvnorm(n = 1000, mu3, Sigma3)
v <- matrix(nrow=3000, ncol=D) # validation data
v[1:1000,]<-dat1
v[1001:2000,]<-dat2
v[2001:3000,]<-dat3
z <- matrix(nrow=3000, ncol=K)

vllik <- 0
for(n in 1:3000) {
  for(k in 1:K) {
    z[n,k] <- pi[k]*dmvnorm(v[n,],mu[k,],Sigma[,,k])
  }
  
  #Log likelihood computation.
  vllik <- vllik + log(sum(z[n,]))
}

pi
mu
Sigma

llik[it]
BIC
vllik
K

# BIC for K=2: -1547.347
# BIC for K=3: -1535.766
# BIC for K=4: -1547.366
# So, K=3 wins.

# Validation log lik for K=2: -15225.83
# Validation log lik for K=3: -14903.88
# Validation log lik for K=4: -14982.4
# So, K=3 wins.

# It does NOT make sense to use the log lik on the TRAINING data to select among models because the higher
# the number of components the higher the log lik (since the models are nested). However, using the log
# lik on some VALIDATION makes sense to select among different number of components, since it measures the
# generalization ability of the models on some previously unseen data. This implicitly penalizes models with
# many components because they overfit to the training data. The previous results confirm this.

############################################################################################
# SVMs: Generalization error: For TDDE01 only
############################################################################################

library(kernlab)
set.seed(1234567890)

data(spam)

# Two inner and outer folds. Use one outer fold for training and the other for testing. Training can occur only
# after we have decided the value of C. To do so, we split the training fold into two inner folds. We train C=1,0.1
# in one of the inner folds and validate in the other. We choose the C value that performs best and test it on
# the second outer fold. We swap folds in the process to get a cross-validation scheme. One can instead use the
# argument "cross" from the function "ksvm" to perform the inner cross-validation step.

index <- sample(1:4601)
f1 <- spam[index[1:2300], ]
f11 <- f1[1:1150, ]
f12 <- f1[1151:2300, ]
f2 <- spam[index[2301:4601], ]
f21 <- f2[1:1150, ]
f22 <- f2[1151:2301, ]

filter <- ksvm(type~.,data=f11,kernel="rbfdot",kpar=list(sigma=0.05),C=1)
mailtype <- predict(filter,f12[,-58])
t <- table(mailtype,f12[,58])
(t[1,2]+t[2,1])/sum(t)

# Error = 0.11

filter <- ksvm(type~.,data=f12,kernel="rbfdot",kpar=list(sigma=0.05),C=1)
mailtype <- predict(filter,f11[,-58])
t <- table(mailtype,f11[,58])
(t[1,2]+t[2,1])/sum(t)

# Error = 0.08

filter <- ksvm(type~.,data=f11,kernel="rbfdot",kpar=list(sigma=0.05),C=0.1)
mailtype <- predict(filter,f12[,-58])
t <- table(mailtype,f12[,58])
(t[1,2]+t[2,1])/sum(t)

# Error = 0.21

filter <- ksvm(type~.,data=f12,kernel="rbfdot",kpar=list(sigma=0.05),C=0.1)
mailtype <- predict(filter,f11[,-58])
t <- table(mailtype,f11[,58])
(t[1,2]+t[2,1])/sum(t)

# Error = 0.16

# 0.11+0.08 vs 0.21+0.16. So, C=1 wins.

filter <- ksvm(type~.,data=f1,kernel="rbfdot",kpar=list(sigma=0.05),C=1)
mailtype <- predict(filter,f2[,-58])
t <- table(mailtype,f2[,58])
(t[1,2]+t[2,1])/sum(t)

# Generalization error (training=f1,test=f2) = 0.08.

filter <- ksvm(type~.,data=f21,kernel="rbfdot",kpar=list(sigma=0.05),C=1)
mailtype <- predict(filter,f22[,-58])
t <- table(mailtype,f22[,58])
(t[1,2]+t[2,1])/sum(t)

# Error = 0.10

filter <- ksvm(type~.,data=f22,kernel="rbfdot",kpar=list(sigma=0.05),C=1)
mailtype <- predict(filter,f21[,-58])
t <- table(mailtype,f21[,58])
(t[1,2]+t[2,1])/sum(t)

# Error = 0.09

filter <- ksvm(type~.,data=f21,kernel="rbfdot",kpar=list(sigma=0.05),C=0.1)
mailtype <- predict(filter,f22[,-58])
t <- table(mailtype,f22[,58])
(t[1,2]+t[2,1])/sum(t)

# Error = 0.23

filter <- ksvm(type~.,data=f22,kernel="rbfdot",kpar=list(sigma=0.05),C=0.1)
mailtype <- predict(filter,f21[,-58])
t <- table(mailtype,f21[,58])
(t[1,2]+t[2,1])/sum(t)

# Error = 0.14

# 0.10+0.09 vs 0.23+0.14. So, C=1 wins.

filter <- ksvm(type~.,data=f2,kernel="rbfdot",kpar=list(sigma=0.05),C=1)
mailtype <- predict(filter,f1[,-58])
t <- table(mailtype,f1[,58])
(t[1,2]+t[2,1])/sum(t)

# Generalization error (training=f2,test=f1) = 0.08.

# Answer: Generalization error = 0.08 + 0.08.

############################################################################################
# NNs: Generalization error: For TDDE01 only
############################################################################################

# First, my solution to the lab.

library(neuralnet)
set.seed(1234567890)

Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

# plot(trva)
# plot(tr)
# plot(va)

restr <- vector(length = 10)
resva <- vector(length = 10)
winit <- runif(31, -1, 1) # Random initializaiton of the weights in the interval [-1, 1]
for(i in 1:10) {
  nn <- neuralnet(formula = Sin ~ Var, data = tr, hidden = 10, startweights = winit,
                  threshold = i/1000, lifesign = "full")
  
  # nn$result.matrix
  
  aux <- predict(nn, tr) # Compute predictions for the trainig set and their squared error
  restr[i] <- sum((tr[,2] - aux)**2)/2
  
  aux <- predict(nn, va) # The same for the validation set
  resva[i] <- sum((va[,2] - aux)**2)/2
}
plot(restr, type = "o")
plot(resva, type = "o")
restr
resva

# The graphs show an example of overfitting, i.e. the threshold that achieves the lowest squared error
# in the training set is not the one that achieves the lowest error in the validation set. Therefore, 
# early stopping is necessary, i.e. running gradient descent until convergence is not the best option,
# as the lowest threshold gives the best error in the training set but not in the validation set.
# Specifically, the validation set indicates that gradient descent should be stoped when 
# threshold = 4/1000. So, the output should be a NN learnt with all (!) the data available and the
# threshold = 4/1000.

winit <- runif(31, -1, 1)
plot(nn <- neuralnet(formula = Sin ~ Var, data = trva, hidden = 10, startweights = winit,
                     threshold = 4/1000, lifesign = "full"))

aux <- predict(nn, trva) # Compute predictions for the training set and their squared error
restrva <- sum((trva[,2] - aux)**2)/2

# Now, my solution to the exam. Since all the data has been used selecting the appropriate threshold, I need
# new data to estimate the generalization error. So, I generate more data. The generalization error is always
# computed from previously unseen data.

Var <- runif(5000, 0, 10)
te <- data.frame(Var, Sin=sin(Var))

aux <- predict(nn, te) # Compute predictions for the test set and their squared error
reste <- sum((te[,2] - aux)**2)/2

restrva/50 # Per case performance on the training data. Just for comparison.
reste/5000 # Per case performance on the test data. This is the estimate of the generalization error.

# It would also be fine to split the data used in the lab in training and test. Then, split again the training
# data into training and validation, and use these sets to select the threshold. Finally, use the test set to
# estimate the generalization error. This solution does not require producing new data. In either case, the
# key observation is that the generalization error is computed from previously unseen data.

############################################################################################
# NNs: Weight interpretation: For both TDDE01 and 732A99
############################################################################################

library(neuralnet)
set.seed(1234567890)

x1 <- runif(1000, -1, 1)
x2 <- runif(1000, -1, 1)
tr <- data.frame(x1,x2, y=x1 + x2)

winit <- runif(9, -1, 1)
nn<-neuralnet(formula = y ~ x1 + x2, data = tr, hidden = c(1), act.fct = "tanh")
plot(nn)

# y=x1+x2 implis that y=7.75(x1/7.75+x2/7.75)=7.75(0.13*x1+0.13*x2). Therefore, by choosing the weights as the NN
# does, the hidden unit takes the value resulting from passing 0.13*x1+0.13*x2 through tanh. 
# However, 0.13*x1+0.13*x2 is small for the training data at hand and, thus, tanh behaves linearly (run the code
# below to appreciate this). Therefore, the hidden unit equals x1/7.75+x2/7.75 and, now, it only remains to 
# weighten this with 7.75 to produce the output x1+x2.

f <- seq(-2,2,.1)
g <- tanh(f)
plot(f,g,type="l")
v <- tanh(0.13*tr$x1+0.13*tr$x2)
points(0.13*tr$x1+0.13*tr$x2,v) # The hidden unit only takes values in the linear part of the tanh.
