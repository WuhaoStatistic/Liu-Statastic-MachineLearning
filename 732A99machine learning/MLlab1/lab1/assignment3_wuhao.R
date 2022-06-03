# assignment 3
library(ggplot2)
################################################################################
# rename columns
#
# f1 : Number of times pregnant.  
# f2 : Plasma glucose concentration a 2 hours in an oral glucose tolerance test.
# f3 : Diastolic blood pressure (mm Hg).
# f4 : Triceps skinfold thickness (mm).
# f5 : 2-Hour serum insulin (mu U/ml).
# f6 : Body mass index (weight in kg/(height in m)^2).
# f7 : Diabetes pedigree function.
# f8 : Age (years).
# target : Diabetes (0=no or 1=yes)
data <- read.csv('pima-indians-diabetes.csv',header = FALSE)
data[,9] <- as.factor(data[,9])
for_rename <- 1:8
for_rename <- as.character(for_rename)
for_rename <- paste0('f',for_rename)
for_rename <- c(for_rename,'target')
colnames(data) <- for_rename
rm(for_rename)
################################################################################
# question 1
# pgc is Plasma glucose concentration a 2 hours in an oral glucose tolerance test.
# type is Diabetes (0=no or 1=yes)
# make data.frame for plotting
pgc <- data[,'f2']
age <- data[,'f8']
type <- data[,'target']
df <- data.frame(pgc=pgc,age=age,type = type)
# plot
p1 <-ggplot(df,aes(x=age,y=pgc))+
  geom_point(aes(color=type))+
  ggtitle(' Diabetes levels : Plasma glucose concentration on Age ')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
################################################################################
# q2
# we need numeric result(probability), so change this column(from factor) into numeric
data[,9] <- as.numeric(as.character(data[,9]))
# build model
glm_q2 <- glm(target~f2+f8,data = as.data.frame(data),family = 'binomial')
# do prediction
prob<-predict(object =glm_q2,newdata=data,type = "response")
# using the threshold 0.5 to do classification
names(prob) <- c()
pred<-ifelse(prob>=0.5,1,0)
# compute error rate 0.266
error_rate <- sum(abs(data[,9]-pred))/length(data[,9])
# plotting
pred <- as.factor(pred)
df <- data.frame(pgc=pgc,age=age,pred = pred)
p2 <-ggplot(df,aes(x=age,y=pgc))+
  geom_point(aes(color=pred))+
  ggtitle(' predicted graph based on age and PGC ')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
# equation is -5.89785793 + PGC*.0.03558250 + age*0.02450157
################################################################################
#q3
glm_q3<-step(object = glm_q2,trace = 0)
# equation is PGC = 179.8034969 - 0.6896993666*age
p3 <-ggplot(df,aes(x=age,y=pgc))+
  geom_point(aes(color=pred))+
  ggtitle(' predicted graph based on age and PGC ')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))+
  geom_abline(slope = -0.6896993666,intercept =  179.8034969) # 165 fitted best
################################################################################
# q4

# we get prob from question 2 using the result from glm_q2
pred_0.2<-ifelse(prob>=0.2,1,0)
pred_0.8<-ifelse(prob>=0.8,1,0)

pred_0.2 <- as.factor(pred_0.2)
df <- data.frame(pgc=pgc,age=age,pred = pred_0.2)
p4_1 <-ggplot(df,aes(x=age,y=pgc))+
  geom_point(aes(color=pred))+
  ggtitle(' predicted graph based on age and PGC ,threshold 0.2')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))

pred_0.8 <- as.factor(pred_0.8)
df <- data.frame(pgc=pgc,age=age,pred = pred_0.8)
p4_2 <-ggplot(df,aes(x=age,y=pgc))+
  geom_point(aes(color=pred))+
  ggtitle(' predicted graph based on age and PGC ,threshold 0.8')+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
################################################################################
#q5
# build new features
x1 <- data[,'f1']
x2 <- data[,'f2']
z1 <- x1**4
z2 <- x2*x1**3
z3 <- x1**2*x2**2
z4 <- x1*x2**3
z5 <- x2**4
target <- as.factor(data[,9])
# build new data.set
dataset <- data.frame(x1=x1,x2=x2,z1=z1,z2=z2,z3=z3,z4=z4,z5=z5,target=target)
# modeling and prediction
glm_q5 <- glm(target~.,data = dataset,family = 'binomial')
prob_q5<-predict(object =glm_q5,newdata=data,type = "response")
names(prob_q5) <- c()
pred_q5<-ifelse(prob_q5>=0.5,1,0)
# compute error rate 0.258
target <- as.numeric(as.character(target))
error_rate_q5 <- sum(abs(target-pred_q5))/length(target)

























