library(reshape2)
library(ggplot2)
f1 <- function(x)
{
  return(x**2/exp(x)-2*exp(-9*sin(x)/(x**2+x+1)))
}

crossover <- function(x,y)
{
  return((x+y)/2)
}

mutate <- function(x)
{
  x**2%%30
}

f4 <- function(maxiter,mutprob)
{
  plot(0:30,f1(0:30),main = 'plot for question 4(a)')
  X <- 0:6*5
  values <- f1(X)
  extend <- c()
  for (i in 1:maxiter) 
  {
    parents <- sample(X,2)
    victim_index <- order(values,decreasing = FALSE)[1]
    kid <- crossover(parents[1],parents[2])
    if(runif(1) >= 1-mutprob)
      kid <- mutate(kid)
    values[victim_index] <- f1(kid)
    X[victim_index] <- kid
    extend <- c(extend,max(values))
  }
  df <- data.frame(index = 0:(30+length(extend)),value = c(f1(0:30),extend),color = c(0:30-0:30,1:(length(extend))-0:(length(extend)-1)))
  p1<- ggplot(df,
         aes(x=index,y=value,color=as.factor(color)))+
    geom_point()+
    xlab("index")+
    ylab("value")+
    ggtitle(paste0('genetic plot with maxiter:',maxiter,' mutprob: ',mutprob))+
    theme(plot.title = ggplot2::element_text(hjust=0.5))
  print(p1)
  print(X)
}
f4(100,0.9)
