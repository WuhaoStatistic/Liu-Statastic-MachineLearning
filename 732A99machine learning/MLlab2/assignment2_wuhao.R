library(tree)
library(rpart)
library(ggplot2)
library(reshape2)
library(pROC)
############################################################################
# question 1
############################################################################
data <- read.csv('bank-full_change.csv',stringsAsFactors = TRUE)
data <- data[,-12]
set.seed(12345)
n <- dim(data)[1]
id <- sample(1:n, floor(n*0.4)) 
train <- data[id,] 
set.seed(12345)
id1 <- setdiff(1:n, id)
id2 <- sample(id1, floor(n*0.3)) 
valid <- data[id2,]
id3 <- setdiff(id1,id2)
test <- data[id3,]
rm(id,id1,id2,id3,n)
############################################################################
# question 2
############################################################################
# mincut ：if current nodes wants to splitm, then all child nodes must have mincut observation
# minsize ： all nodes must have at least minsize observation
default_tree <- tree(y~.,data=train,split=c('deviance'))

pre_default <- predict(default_tree,newdata = valid,type='class')
valid_mis_def  <- mean(valid[,16]!=pre_default) 

train_pre_def <- predict(default_tree,newdata = train,type='class')
train_mis_def  <- mean(train[,16]!=train_pre_def) 
###############
node_tree <- tree(y~.,train,control = tree.control(nobs=18084,minsize = 7000))

pre_node <- predict(node_tree,newdata = valid,type='class')
valid_mis_node <- mean(valid[,16]!=pre_node) 

train_pre_node <- predict(node_tree,newdata = train,type='class')
train_mis_node  <- mean(train[,16]!=train_pre_def) 
###############
deviance_tree <- tree(y~.,train,mindev=0.0005)
pre_dev <- predict(deviance_tree,newdata = valid,type='class')
valid_mis_dev  <- mean(valid[,16]!=pre_dev) 
train_pre_dev <- predict(deviance_tree,newdata = train,type='class')
train_mis_dev  <- mean(train[,16]!=train_pre_dev) 
rm(pre_default,pre_dev,pre_node,train_pre_def,train_pre_dev,train_pre_node)
rm(default_tree,node_tree)
# bigger minimum deviance will lead to a bigger tree. Since the node is more easy to split
# bigger smallest allowed node size will let every single node contains more observations,thus leading less nodes and a smaller tree.
# minsize = 7000 is best
############################################################################
# question 3
############################################################################
optim_map <- data.frame(matrix(c(0,0,0),nrow = 49,ncol = 3))
for (i in 2:50) {
  ptree <- prune.tree(deviance_tree,best=i,newdata = valid)
  pre_1 <- predict(ptree,type="tree")
  pre_2 <- predict(ptree,newdata=valid,type="tree")
  optim_map[i-1,] <- c(i, deviance(pre_1),deviance(pre_2))
}
colnames(optim_map) <- c('number_leaves','deviance_train','deviance_valid')
# best number leaves 22
best_number_leaves <- optim_map[order(optim_map[,'deviance_valid'])[1],'number_leaves']
# according to the optim_map, the best leaves number is 22.
opt_melt <- melt(as.data.frame(optim_map), id="number_leaves")

ggplot(data=as.data.frame(opt_melt),
       aes(x=number_leaves,y=value,color=variable))+
  geom_line()+
  geom_point()+
  xlab("number of leaves")+
  ylab("deviance")

best_tree <- prune.tree(deviance_tree,best=best_number_leaves,newdata = valid)
summary(best_tree)
#from summary , we can see these parameters are important.
#  "poutcome" "month"    "contact"  "pdays"    "age"      "day"      "balance"  "housing"  "job"   
############################################################################
# question 4
############################################################################
pre_test <- predict(best_tree,newdata=test,type = 'class')
confu <- table(test[,16],pre_test)
f1_score <- 2*confu[2,2]/(2*confu[2,2]+confu[1,2]+confu[2,1])
accura <- 1 - mean(pre_test != test[,16]) 
# Since the cost of false negative and false positive should actually be different,
# so simple accuracy is not good here, since most observations are labeled 'no',
# and this highly tilted training data would also lead a unbalanced prediction result.
############################################################################
# question 5
############################################################################
# if false negative 5 
# if false possitive 1
loss_matrix <- matrix(c(0,5,1,0),2,2)

q5_tree <- prune.tree(deviance_tree,best=22,newdata = valid,loss = loss_matrix)

q5_test_pre <- predict(q5_tree,newdata = test,type='class')

confu_q5 <- table(test[,16],q5_test_pre)
f1_score_q5 <- 2*confu_q5[1,1]/(2*confu_q5[1,1]+confu_q5[1,2]+confu_q5[2,1])
accura_q5 <- 1 - mean(q5_test_pre != test[,16]) 
############################################################################
# question 6
############################################################################
q6_glm <- glm(y~.,data = train,family = 'binomial')
pre_test <- predict(q6_glm,newdata=test,type = 'response') 
df <- data.frame(prob = pre_test,obs =test[,16])
glm_roc <- roc(test[,16],pre_test)
plot(glm_roc,print.auc = TRUE,print.thres=TRUE,main = 'roc curve for glm')

pre_test_tree <- predict(best_tree,newdata=test,type='vector')[,2]
df <- data.frame(prob = pre_test_tree,obs =test[,16])
tree_roc <- roc(test[,16],pre_test_tree)
plot(tree_roc,print.auc = TRUE,print.thres=TRUE,main = 'roc curve for tree')

tpr_tree <- 1:19
fpr_tree <- 1:19
tpr_glm  <- 1:19
fpr_glm  <- 1:19 
p_tree <- 1:19
r_tree <- 1:19
p_glm <- 1:19
r_glm <- 1:19

for (i in 1:19/20) 
{
  tp_tree <- sum(pre_test_tree>i & test[,16]=='yes')
  tn_tree <- sum(pre_test_tree<i & test[,16]=='no')
  fp_tree <- sum(pre_test_tree>i & test[,16]=='no')
  fn_tree <-  sum(pre_test_tree<i & test[,16]=='yes')
  
  tp_glm  <- sum(pre_test>i & test[,16]=='yes')
  fp_glm  <- sum(pre_test>i & test[,16]=='no')
  tn_glm  <- sum(pre_test<i & test[,16]=='no')
  fn_glm  <- sum(pre_test<i & test[,16]=='yes')
  
  tpr_tree[i*20] <- tp_tree/(tp_tree+fn_tree)
  fpr_tree[i*20] <- fp_tree/(tn_tree+fp_tree)
  
  tpr_glm[i*20] <- tp_glm/(tp_glm+fn_glm)
  fpr_glm[i*20] <- fp_glm/(tn_glm+fp_glm)
  
  p_tree[i*20] <- tp_tree/(tp_tree+fp_tree)
  p_glm[i*20] <- tp_glm/(tp_glm+fp_glm)

}
r_tree <- tpr_tree
r_glm <- tpr_glm
plot(r_tree,p_tree,main='PR curve for tree')
plot(r_glm,p_glm,main='PR curve for glm')
# aksgu350








