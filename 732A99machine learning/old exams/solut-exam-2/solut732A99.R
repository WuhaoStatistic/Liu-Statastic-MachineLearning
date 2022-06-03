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

df=read.csv2("mortality_rate.csv")
plot(df$Day, df$Rate, cex=0.5)

X=df$Day
df1=data.frame(X1=X,X2=X^2, X3=X^3, X4=X^4, X5=ifelse(X-75>0, X-75,0)^4, Y=df$Rate)
m2=lm(Y~., df1)
summary(m2)
Pr=predict(m2)
points(df$Day, Pr, col="blue", cex=0.5)

#Degrees of freedom 6

#3


data0=read.csv("geneexp.csv")

data=data0
data$CellType=c()
x=t(data)
data$CellType=as.factor(data0$CellType)
library(pamr)
rownames(data)=1:nrow(data)
y=data0$CellType
mydata=list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=rownames(x))
model=pamr.train(mydata,threshold=seq(0,4, 0.1))
set.seed(12345)
cvmodel=pamr.cv(model,mydata)
print(cvmodel)
#optimal 3.3 

pamr.plotcen(model, mydata, threshold=3.3)
a=pamr.listgenes(model,mydata,threshold=3.3)
cat( paste( colnames(data)[as.numeric(a[1:5,1])], collapse='\n' ) )

print(nrow(a))

