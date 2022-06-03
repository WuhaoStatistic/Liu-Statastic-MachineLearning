library(ggplot2)
library(kknn)
library(reshape2)
# Assignment 1 KNN-method for hand-writing
################################
# loading data 
data <- read.csv('optdigits.csv',header = FALSE)
################################################################################
# pre-processing (change label into factor and rename columns)
data[,65] <- as.factor(data[,65])
for_rename <- 1:64
for_rename <- as.character(for_rename)
for_rename <- paste0('f',for_rename)
for_rename <- c(for_rename,'target')
colnames(data) <- for_rename
################################################################################
# Question 1
# doing partition(from lecture 1a block 1.pdf page 31)
set.seed(12345)
n <- dim(data)[1]
id <- sample(1:n, floor(n*0.5)) 
train <- data[id,] 
set.seed(12345)
id1 <- setdiff(1:n, id)
id2 <- sample(id1, floor(n*0.25)) 
valid <- data[id2,]
set.seed(12345)
id3 <- setdiff(id1,id2)
test <- data[id3,]
rm(id,id1,id2,id3,n,for_rename)
################################################################################
# Question 2 
# fit the model
kn <- train.kknn(target~.,data = train,ks = 30,kernel =  'rectangular')
# evaluate in the train data
pred_train <- predict(kn,train)
t_train <- table(train$target,pred_train)
# evaluate in the test data
pred_test <- predict(kn,test)
t_test <- table(test$target,pred_test)
# calculate the error rate
# for train data
error_rate_train <- 1-sum(diag(t_train))/sum(t_train) # 0.042
# for test data
error_rate_test <- 1-sum(diag(t_test))/sum(t_test) # 0.049

################################################################################
#Question 3
# train the model and fit the model to training data to get the probability.
wri_knn <- kknn(target~.,train = train,test = train,kernel = 'rectangular',k=30)
pre_knn <- predict(wri_knn)
# index_list_of_8 contains all the observations whose prediction and label are both 8.
index_list_of_8 <- which(train[,65]==8)[which(which(train[,65]==8)%in%which(pre_knn==8))] 
# these two vector contains the probability of lowest and highest probability 
lowest_prob <- sort(wri_knn[['prob']][index_list_of_8,9])[1:3]
highest_prob <- sort(wri_knn[['prob']][index_list_of_8,9],decreasing = TRUE)[1:2]
# find the most 2 difficult observations
lowest_pro_index <- index_list_of_8[which(wri_knn[['prob']][index_list_of_8,9]%in%lowest_prob)][1:3]
# find the 3 easiest observations
highest_pro_index <- index_list_of_8[which(wri_knn[['prob']][index_list_of_8,9]%in%highest_prob)][1:2]
# get the pixel map of good and bad
pixelMapsBad <- train[lowest_pro_index,-65]
pixelMapsGood <- train[highest_pro_index,-65]
# restore the graphic, using heatmap() to view.
figGoodEight1<- matrix(unlist(pixelMapsGood[1,], use.names = FALSE), ncol = 8, nrow = 8, byrow = TRUE)
figGoodEight2<- matrix(unlist(pixelMapsGood[2,], use.names = FALSE), ncol = 8, nrow = 8, byrow = TRUE)
figBadEight1 <- matrix(unlist(pixelMapsBad[1,], use.names = FALSE), ncol = 8, nrow = 8, byrow = TRUE)
figBadEight2 <- matrix(unlist(pixelMapsBad[2,], use.names = FALSE), ncol = 8, nrow = 8, byrow = TRUE)
figBadEight3 <- matrix(unlist(pixelMapsBad[3,], use.names = FALSE), ncol = 8, nrow = 8, byrow = TRUE)
heatmap(figGoodEight2, Colv = NA, Rowv = NA)
# comments:
# All of them seems easier to identify in this way. 
################################################################################
#Question 4
kx <- 1:30
error_train <- c()
error_valid <- c()
for(i in 1:30)
{
  # model training
  kn <- train.kknn(target~.,data = train,ks = i,kernel = "rectangular")
  # predication
  predic_train <- predict(kn,train)
  predic_valid <- predict(kn,valid)
  # make confusion matrix
  t_train <- table(train$target,predic_train,dnn = c('true','predict'))
  t_valid <- table(valid$target,predic_valid,dnn = c('true','predict'))
  # compute error rate
  error_rate_train <- 1-sum(diag(t_train))/sum(t_train)
  error_rate_valid <- 1-sum(diag(t_valid))/sum(t_valid)
  # add error_rate into a vector for drawing graph
  error_train <- c(error_train,error_rate_train)
  error_valid <- c(error_valid,error_rate_valid)
  # remove redundant data to save memory
  rm(predic_train,predic_valid,error_rate_train,error_rate_valid)
}
# gathering data into a data.frame to apply ggplot2
df <- data.frame(k = 1:30,error_train=error_train,error_valid=error_valid)
# reshape data so that 2 plot lines can be plotted in a single graph
df1 <- melt(df,id.vars='k')
# plot
p1 <-ggplot(df1,aes(x=k,y=value))+
    geom_point(aes(color=variable))+
    ggtitle('error_rate : valid vs train')+
    theme(plot.title = ggplot2::element_text(hjust=0.5))
# from the graph the optimal k is 7
kbest <- train.kknn(target~.,data = train,kernel = 'rectangular',ks=7)
pred_test <- predict(kbest,newdata = test)
t <- table(test$target,pred_test)
error_rate_test <- 1-sum(diag(t))/sum(t)
# remove redundant data
rm(df,df1,pred_test,kbest)
################################################################################
#Question 5
ent_list <- c()
for(i in 1:30)
{
  kn <- kknn(target~.,train = train,test = valid,kernel = 'rectangular',k=i)
  ent <- 0
  for(j in 1:length(valid[,1]))
  {  
     col_index <- as.numeric(as.character(valid[j,65]))+1
     ent <- ent - log(as.numeric(as.character((kn[['prob']][j,col_index])))+1e-15)
  }
  ent_list <- c(ent_list,ent)
}
# make df to draw graph
df <- data.frame(k = 1:30,cross_entropy = ent_list)
p5entro <-ggplot(df,aes(x=k,y=cross_entropy))+
          geom_point(color=7,size = 3)+
          ggtitle('cross-entropy depends on k')+
          theme(plot.title = ggplot2::element_text(hjust=0.5))
print(p5entro)

















