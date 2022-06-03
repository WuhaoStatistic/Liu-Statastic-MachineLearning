library("boot")
stat1<-function(data,vn){
    data<-as.data.frame(data[vn,])
    res<-lm(Response~Predictor,data)
    res$coefficients[2]
}
x<-rnorm(100);data<-cbind(Predictor=x,Response=3+2*x+rnorm(length(x),sd=0.5))
res<-boot(data,stat1,R=1000)
print(boot.ci(res))
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
##Based on 1000 bootstrap replicates
#Intervals : 
#Level      Normal              Basic         
#95%   ( 1.933,  2.164 )   ( 1.935,  2.162 )  
# Level     Percentile            BCa          
#95%   ( 1.934,  2.161 )   ( 1.936,  2.166 )  
