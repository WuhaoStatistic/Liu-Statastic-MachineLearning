data=read.csv("glass.csv")
RNGversion("3.5.1")
data$ID=c()
data$Class=as.factor(data$Class)

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.4))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.3))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

combined=rbind(train,valid)
m1=glm(Class~., data=combined, family=binomial)

print(m1)
Pred1=predict(m1, test, type="response")
PredC=as.numeric(Pred1>0.5)
table(test$Class, PredC)

#2
library(tree)
fit=tree(as.numeric(Class)~., data=train,)
trainScore=rep(0,4)
testScore=rep(0,4)
for(i in 2:4) {
  prunedTree=prune.tree(fit,best=i)
  pred=predict(prunedTree, newdata=valid,
               type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}
plot(2:4, trainScore[2:4], type="b", col="red",
     ylim=c(0,250))
points(2:4, testScore[2:4], type="b", col="blue")

finalTree=prune.tree(fit,best = 3)
Pred2=predict(finalTree, test, "vector")

PredC2=as.numeric(Pred2[,2]>=0.5)
table(test$Class, PredC2)

PredC3=as.numeric((Pred2[,2]+Pred1)/2>0.5)
table(test$Class, PredC3)

#U3

df=read.csv2("popularkids.csv")

poster=function(df, cond){
  df1=df[df$Goals==cond,]
  n1=nrow(df1)
  n=nrow(df)
  
  p1=length(which(df1$Gender=="boy"))/n1
  p2=length(which(df1$Grade == 6))/n1
  p3=length(which(df1$School == "Elm"))/n1
  prior=n1/n
  print(c(p1, p2, p3, prior))
  
  return(p1*p2*p3*prior)
}

poster(df,"Grades")
poster(df,"Popular")
poster(df,"Sports")

#Popular.

