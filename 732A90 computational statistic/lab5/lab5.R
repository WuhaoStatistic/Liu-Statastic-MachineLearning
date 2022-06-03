library(boot)
library(ggplot2)
data <- read.csv2('lottery.csv')
df <- data.frame(x = data[,4],y = data[,5])
los <- loess(y~x,data = df)
y_hat <- los[['fitted']]
max_x <- which(y_hat==max(y_hat))
min_x <- which(y_hat==min(y_hat))
T_ <- (max(y_hat)-min(y_hat))/(max_x-min_x)

stats <- function(data,vec){
  datatemp<-data[vec,]
  los1 <- loess(y ~ x, data = datatemp)
  y_h <- los1[['fitted']] 
  Xb = which.max(y_h)[1]
  Xa = which.min(y_h)[1]
  
  fit_Xb = y_h[Xb]
  fit_Xa = y_h[Xa]
  
  T = (fit_Xb - fit_Xa) / (Xb-Xa)
  return(T)
}
set.seed(12345)
myboot = boot::boot(data = df, 
              statistic = stats,
              R = 2000)
# summary
myboot
# plot distribution
# plot(myboot, index = 1)
df <- data.frame(t=myboot$t)
per95 = sort(myboot$t)[1950]
ggplot(data = df, aes(x = t)) + 
  ggtitle("Histogram of t") + 
  geom_histogram(aes(y=..density..),
                 colour="black",
                 fill="white",
                 bins=30) + 
  geom_vline(aes(xintercept = per95, color = "red"),size=1.5)+
  theme(plot.title = ggplot2::element_text(hjust=0.5))
############################################################################
#Question 2
############################################################################
rm(list=ls())
data <- read.csv2('prices1.csv')
df <- data.frame(p = data[,1])
ggplot(data = df, aes(x = p)) + 
  ggtitle("Histogram of price") + 
  geom_histogram(aes(y=..density..),
                 colour="black",
                 fill="white",
                 bins=30) + 
  theme(plot.title = ggplot2::element_text(hjust=0.5))
print()

stat1 <- function(vec,vn){
  return(mean(vec[vn]))
}
B=1000
set.seed(12345)
res1 = boot(data$Price, stat1, R=B)
res1
2*res1$t0-mean(res1$t)
# variance of mean price (output of statistic)
var_boot <- 1/(B-1)*sum((res1$t-mean(res1$t))^2 )
var_boot
# default is a 95% confidence interval
ci <- boot.ci(res1, type = c("perc", "bca", "norm"))
print(ci) 
plot(res1)
mean()