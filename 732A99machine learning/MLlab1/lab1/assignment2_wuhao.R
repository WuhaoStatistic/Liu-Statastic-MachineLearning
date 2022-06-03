#assignment 2
################################################################################
#Question 1
# read data and pre-processing
data <- read.csv('parkinsons.csv')
data <- scale(data)
# drop useless features(not mentioned in the pdf)
data <- data[,5:length(colnames(data))]
data <- as.data.frame(data[,-2])
# split data into 2 parts
set.seed(12345)
n <- dim(data)[1]
id <- sample(1:n, floor(n*0.6)) 
train <- data[id,] 
set.seed(12345)
id1 <- setdiff(1:n, id)
test <- data[id1,]
# remove redundant variable
rm(id,id1,n)
################################################################################
#Question 2
# fit the model
lm_parkinsons <- lm(motor_UPDRS~.-1,data = train)
# do prediction 
pred_test <- predict.lm(lm_parkinsons,test)
# compute MSE for test data
MSE_test <- sum((test['motor_UPDRS']-pred_test)*(test['motor_UPDRS']-pred_test))/nrow(test)
# 
# using the summary function and we find that some variable have 3 stars which
# means they contribute a lot to the model.
################################################################################
#Question3
# data is the training data without the label so it will be data[,-1]
# response is the target variable so it will be data[,1]

likelihood <- function(sita,sigma,data,response)
{
  x <-as.matrix(data)
  y <- as.numeric(response)
  n <- dim(data)[1]
  part1 <- -n/2*log(2*pi*sigma**2)
  part2 <- -sum(((sita%*%t(x)-y)/sigma)**2)/2
  return(part1+part2)
}


# using this data as the initial data for optim in the next question
Ridge <- function(par,data,response,scalar)
{
  res <- likelihood(par[-17],par[17],data,response)
  return(-res+scalar*sum(par[-17]**2))
}

RidgeOpt <- function(par,data,response,scalar,fn)
{
  fn1 <- function(par,data,response,scalar) fn(par,data,response,scalar)

  res <- optim(par = par,fn=fn1,data=data,response=response,scalar=scalar,method = 'BFGS')
  return(res)  
}

DF<-function(lambda,input_data){
  
  X<-as.matrix(train[, -17])
  
  deg_of_free<-sum(diag( (X %*% solve(t(X) %*% X + lambda*diag(16))) %*% t(X) ) )
  
  return(deg_of_free)
}

sita <- lm_parkinsons[['coefficients']]
sigma <- 0.9366
r1 <- RidgeOpt(c(sita,sigma),train[,-1],train[,1],scalar = 1,fn=Ridge)

pred_test_1 <- r1[['par']][1:16]%*%t(test[,-1])
pred_train_1 <- r1[['par']][1:16]%*%t(train[,-1])

MSE_train_1 <- mean((train[,'motor_UPDRS']-pred_train_1)**2)
MSE_TEST_1  <- sum((test['motor_UPDRS']-pred_test_1)*(test['motor_UPDRS']-pred_test_1))/nrow(test)
  


sita <- lm_parkinsons[['coefficients']]
sigma <- 0.9366
r100 <- RidgeOpt(c(sita,sigma),train[,-1],train[,1],scalar = 100,fn=Ridge)

pred_test_100 <- r100[['par']][1:16]%*%t(test[,-1])
pred_train_100 <- r100[['par']][1:16]%*%t(train[,-1])
MSE_train_100 <- mean((train[,'motor_UPDRS']-pred_train_100)**2)
MSE_TEST_100  <- sum((test['motor_UPDRS']-pred_test_100)*(test['motor_UPDRS']-pred_test_100))/nrow(test)



sita <- lm_parkinsons[['coefficients']]
sigma <- 0.9366
r1000 <- RidgeOpt(c(sita,sigma),train[,-1],train[,1],scalar = 1000,fn=Ridge)

pred_test_1000 <- r1000[['par']][1:16]%*%t(test[,-1])
pred_train_1000 <- r1000[['par']][1:16]%*%t(train[,-1])
MSE_train_1000 <- sum((train['motor_UPDRS']-pred_train_1000)*(train['motor_UPDRS']-pred_train_1000))/nrow(train)
MSE_TEST_1000  <- sum((test['motor_UPDRS']-pred_test_1000)*(test['motor_UPDRS']-pred_test_1000))/nrow(test)




