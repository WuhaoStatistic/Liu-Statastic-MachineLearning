library(kernlab)
set.seed(1234567890)
data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
train <- spam[1:3000, ]
valid <- spam[3001:3800, ]
train_valid <- spam[1:3800, ]
test <- spam[3801:4601, ] 

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=train,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,valid[,-58])
  t <- table(mailtype,valid[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}

filter0 <- ksvm(type~.,data=train,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,valid[,-58])
t <- table(mailtype,valid[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

filter1 <- ksvm(type~.,data=train,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,test[,-58])
t <- table(mailtype,test[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

filter2 <- ksvm(type~.,data=train_valid,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,test[,-58])
t <- table(mailtype,test[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,test[,-58])
t <- table(mailtype,test[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3
################################################################################
# Question 1 
# filter 3 should be return, since when we find the optimal C, we are using  
# train as training data and valid as test data.
################################################################################
################################################################################
# Question 2
# error 2 should be return to the user. The hyper-parameter C is actually created 
# with train and valid data, so test of generalization error should be done on test data.
################################################################################
spam_matrix <- as.matrix(spam[,-58])
alpha=alphaindex(filter3)[[1]]
support_x <- as.matrix(spam[alpha,-58])
coef <- coef(filter3)[[1]]
b <- b(filter3)

pre_svm <- function(i)
{
  temp <- t(apply(support_x,1,function(x) x-spam_matrix[i,-58]))
  temp <- exp(-0.05*apply(temp**2,1,sum))
  res <- sum(coef*temp)-b
  return(res)
}
my_res <- c()
for (i in 1:10) 
{
  my_res <- c(my_res,pre_svm(i))
}
true <- predict(filter3,spam[1:10,-58],type='decision')
plot(1:10,true,col='red')
lines(1:10,my_res,col='blue')
title('true prediction vs liner combine')

