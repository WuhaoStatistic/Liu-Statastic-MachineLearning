library(reshape2)
library(ggplot2)
# Question 1 
data <- read.csv('communities.csv')
data[,1:100] <- scale(data[,1:100])
eigen_q1 <- eigen(cor(data[,1:100]))
sumv <- sum(eigen_q1[['values']])
curs <- 0 
for (i in 1:length(eigen_q1[['values']])) 
{
    curs <- curs + eigen_q1[['values']][i]
    if(curs > 0.95*sumv)
      break
}
needed_featrues <- i
first_pro <- eigen_q1[['values']][1]/sumv
second_pro <- eigen_q1[['values']][2]/sumv
rm(i,curs,sumv)
###########################################################################
# Question 2 
###########################################################################
# trace plot 
prin_q2 <- princomp(~.,data[,1:100],fix_sign = FALSE)
plot(1:100,prin_q2[['loadings']][,1],main = 'trace plot for first component')
lines(1:100,prin_q2[['loadings']][,1])
colnames(data)[head(order(prin_q2[['loadings']][,1],decreasing = TRUE),5)]
df <- data.frame(pc1 = prin_q2[['scores']][,1],pc2 = prin_q2[['scores']][,2],color = data[,101])
p2 <- ggplot(data=df,aes(x = pc1,y = pc2))+
      geom_point(aes(color = color))+
      ggtitle('pc1 vs pc2')+
      theme(plot.title = element_text(hjust = 0.5))
print(p2)
###########################################################################
# Question 3 
###########################################################################
data <- read.csv('communities.csv')
set.seed(12345)
n <- dim(data)[1]
id <- sample(1:n, floor(n*0.5)) 
train <- data[id,] 
id1 <- setdiff(1:n, id)
test <- data[id1,]
rm(id,id1,n)

train[,1:101] <- scale(train[,1:101])
test[,1:101] <- scale(test[,1:101])

lm_q3 <- lm(ViolentCrimesPerPop~.-1,data=train)

train_error <- mean(lm_q3[['residuals']]**2)
test_error <- mean((predict(lm_q3,test)-test[,101])**2)
###########################################################################
# Question 4 
###########################################################################
# using optim
par <- 1:100-1:100
error_train <- c()
error_test <- c()

cost_function <- function(par,data)
{
  return(sum((data[,101]-as.matrix(data[,1:100])%*%par)**2))
  error_train <- c(error_train,opt[['value']])
  par <- opt[['par']]
  error_test <-c(error_test,cost_function(par,test))
}

for (i in 1:300) 
{
  opt <- optim(par,cost_function,control = list(maxit = 1),method = 'BFGS',data = train)
  error_train <- c(error_train,opt[['value']])
  par <- opt[['par']]
  error_test <-c(error_test,cost_function(par,test))
}

df <- data.frame(sum_error_train = error_train,sum_error_test = error_test,iter = 1:300)
df_melt <- reshape2::melt(df,id.vars='iter')

p1 <-ggplot(df_melt,aes(x=iter,y=value))+
  geom_point(aes(color=variable))+
  ggtitle('error trace plot')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
print(p1)
# according to early stop rules, find iter 28 with lowest error on test dataset.
best_iter <- which.min(error_test) 

par <- 1:100-1:100 
for (i in 1:28) 
{
  best_opt <- optim(par,cost_function,control = list(maxit = 1),method = 'BFGS',data = train)
  par <- best_opt[['par']]
  best_error_test <-cost_function(par,test)
}
best_error_test <- cost_function(best_opt[['par']],test)/length(test[,101])



