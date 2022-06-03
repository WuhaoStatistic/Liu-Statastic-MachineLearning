library(neuralnet)
library(ggplot2)
library(reshape2)
set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test
winit <- rep(0,10)
nn <- neuralnet(Sin~Var,tr,10,startweights = winit)
pre_tr_q1 <- predict(nn,as.data.frame(tr[,-2]))
pre_te_q1 <- predict(nn,as.data.frame(te[,-2]))
plot(tr,cex=2)
points(te,col='blue',cex=1)
points(te[,1],pre_te_q1,col='red',cex=1)
title('plot for question1')
###############################################################################
#q2
###############################################################################
h1 <- function(x) x

h2 <- function(x) ifelse(x>0,x,0)

h3 <- function(x) log(1+exp(x))

########
nn_h1 <- neuralnet(Sin~Var,tr,10,startweights = winit,act.fct = h1)
pre_tr_q21 <- predict(nn_h1,tr)
pre_te_q21 <- predict(nn_h1,te)
plot(tr,cex=2)
points(te,col='blue',cex=1)
points(te[,1],pre_te_q21,col='red',cex=1)
title('plot for question 2_1')
########
nn_h2 <- neuralnet(Sin~Var,tr,10,startweights = winit,act.fct = h2)
pre_tr_q22 <- predict(nn_h2,tr)
pre_te_q22 <- predict(nn_h2,te)
plot(tr,cex=2)
points(te,col='blue',cex=1)
points(te[,1],pre_te_q22,col='red',cex=1)
title('plot for question 2_2')
########
nn_h3 <- neuralnet(Sin~Var,tr,10,startweights = winit,act.fct = h3)
pre_tr_q23 <- predict(nn_h3,tr)
pre_te_q23 <- predict(nn_h3,te)
plot(tr,cex=2)
points(te,col='blue',cex=1)
points(te[,1],pre_te_q23,col='red',cex=1)
title('plot for question 2_3')
###############################################################################
#q3
###############################################################################
set.seed(1234567890)
Var_q3 <- runif(500, 0, 50)
mydata_q3 <- data.frame(Var_q3, Sin=sin(Var_q3))
res_q3 <- predict(nn,newdata = as.data.frame(Var_q3))
plot(mydata_q3,cex=1,col='black')
points(Var_q3,res_q3,col='red')
title('result in question3')
plot(Var_q3,res_q3)
title('convergence condition')
###############################################################################
#q4
###############################################################################
# this is the output of input = 20

sum(c(1,1/(1+exp(-c(1,20)%*%nn$weights[[1]][[1]])))*nn$weights[[1]][[2]])  
# 因为训练集使用的区间是0-10，然而现在我们需要预测0-50，也就是说有一部分的数据是在
# 训练区间之外的，这是任何机器学习模型都无法预测的。而收敛的原因是由于我们使用了sigmoid作为
# 激活函数，对于很大的值，sigmoid会返回接近于-1的值，由于我们有10个神经元，所以总和接近-10.
###############################################################################
#q5
###############################################################################
nn_q5 <- neuralnet(Var~Sin,mydata,10,startweights = winit,threshold = 0.1)








