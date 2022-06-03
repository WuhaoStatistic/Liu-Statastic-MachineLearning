library(ggplot2)
library(reshape2)
library(glmnet)
# Assignment 1 KNN-method for hand-writing
################################
# loading data 
data <- read.csv('tecator.csv')
data <- data[,-1]
data <- data[,c(-102,-103)]
###############################################################################
# doing partition(from lecture 1a block 1.pdf page 31)
set.seed(12345)
n <- dim(data)[1]
id <- sample(1:n, floor(n*0.5)) 
train <- data[id,] 
id1 <- setdiff(1:n, id)
test <- data[id1,]
rm(id,id1,n)
################################################################################
#Question 1
################################################################################
lm_q1 <- lm(Fat~.,data = train)
pre_train <- predict.lm(lm_q1,train)
pre_test  <- predict.lm(lm_q1,test)
error_train <- sum((pre_train-train[,'Fat'])**2)/length(pre_train)
error_test <- sum((pre_test-test[,'Fat'])**2)/length(pre_test)
print('using mean square error')
print(paste0('error for train: ',error_train))
print(paste0('error for test: ',error_test))
# The model is good in training data, but not very good in test data. So, in general
# this model is not good.
################################################################################
#Question 2
################################################################################
#in the Rmarkdown
#in the Rmarkdown
#in the Rmarkdown
#in the Rmarkdown
#in the Rmarkdown
#in the Rmarkdown
################################################################################
#Question 3
################################################################################
gl_0.01 <- glmnet(train[,-101],train[,101],lambda = 0.01)
gl_0.1 <- glmnet(train[,-101],train[,101],lambda = 0.1)
gl_1 <- glmnet(train[,-101],train[,101],lambda = 1)
gl_10 <- glmnet(train[,-101],train[,101],lambda = 10)
df_lambda <-data.frame(channels =1:100,
                       minus_2 = gl_0.01[['beta']][,1],
                       minus_1 = gl_0.1[['beta']][,1],
                       is_0 = gl_1[['beta']][,1],
                       positive_1 = gl_10[['beta']][,1]
                       ) 
df_melt <- reshape2::melt(df_lambda,id.vars='channels')
p1 <-ggplot(df_melt,aes(x=channels,y=value))+
  geom_point(aes(color=variable))+
  ggtitle('coefficitents based on log(lambda),lasso')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
print(p1)
# find lambda which can let model have 3 coefficients being non-zero.
lambda_for_three_coefficients <- 0
lambda_list <- 600:684/1000
for (i in lambda_list) 
{
  gl <- glmnet(train[,-101],train[,101],lambda = i) 
  
  if(length(which(gl[['beta']][,1]!=0))==3)
  {
    lambda_for_three_coefficients <- i
    df_lambda <-data.frame(channels =1:100,
                           value = gl[['beta']][,1])
    p1 <-ggplot(df_lambda,aes(x=channels,y=value))+
      geom_point(color='purple')+
      ggtitle(paste0('lambda = ', i))+
      theme(plot.title = ggplot2::element_text(hjust=0.5))
    print(p1)
    break
  }
}
################################################################################
#Question 4
################################################################################
rg_0.01 <- glmnet(train[,-101],train[,101],lambda = 0.01,alpha = 0)
rg_0.1 <- glmnet(train[,-101],train[,101],lambda = 0.1,alpha = 0)
rg_1 <- glmnet(train[,-101],train[,101],lambda = 1,alpha = 0)
rg_10 <- glmnet(train[,-101],train[,101],lambda = 10,alpha = 0)
df_lambda <-data.frame(channels =1:100,
                       minus_2 = rg_0.01[['beta']][,1],
                       minus_1 = rg_0.1[['beta']][,1],
                       is_0 = rg_1[['beta']][,1],
                       positive_1 = rg_10[['beta']][,1]
) 
df_melt <- reshape2::melt(df_lambda,id.vars='channels')
p1 <-ggplot(df_melt,aes(x=channels,y=value))+
  geom_point(aes(color=variable))+
  ggtitle('coefficitents based on log(lambda),ridge')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
print(p1)
# find lambda which can let model have 3 coefficients being non-zero.
lambda_for_three_coefficients <- 0
lambda_list <- 100:200
for (i in lambda_list) 
{
  rg <- glmnet(train[,-101],train[,101],lambda = i,alpha = 0) 
  if(length(which(rg[['beta']][,1]!=0))==3)
  {
    lambda_for_three_coefficients <- i
    df_lambda <-data.frame(channels =1:100,
                           value = rg[['beta']][,1])
    p1 <-ggplot(df_lambda,aes(x=channels,y=value))+
      geom_point(color='purple')+
      ggtitle(paste0('lambda = ', i))+
      theme(plot.title = ggplot2::element_text(hjust=0.5))
    print(p1)
    break
  }
}
################################################################################
#Question 5
################################################################################
set.seed(12345)
cv <- cv.glmnet(as.matrix(train[,-101]),as.matrix(train[,101]),type.measure = 'mse')

best_lambda <- cv$lambda.min

print(paste0('the best lambda is ',best_lambda,'  log(lambda) = ',log(best_lambda)))

df <- data.frame(log_lambda = log(cv$lambda),cv_mse = cv$cvm)

p1 <- ggplot(data = df,aes(x = log_lambda,y = cv_mse))+
  geom_line(color='purple')+
  ggtitle('cv mse based on log(lambda) ')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
print(p1)

gl <- glmnet(train[,-101],train[,101],lambda = best_lambda)

pre_test <- predict(gl,as.matrix(test[,-101]))[,1]

actual_test <- as.numeric(as.character(test[,101]))

df_test <-data.frame(
                       actual = actual_test,
                       prediction = pre_test
) 
p1 <-ggplot(df_test,aes(x=actual,y=prediction))+
  geom_point(aes(color='red'))+
  ggtitle('best lambda for test,LASSO')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
print(p1)

mean((pre_test-actual_test)**2)
























