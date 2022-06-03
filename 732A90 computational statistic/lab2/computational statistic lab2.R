#computational statistic lab2
library(ggplot2)
library(reshape2)
################################################################################
# Question1
square_error <- function(v,x,a)
{ 
  res <- c()
  for(i in x)
  {
    px <- c(0,i,i**2)
    res <- c(res,t(px)%*%a)
  }
  return(sum((v-res)**2))
}

use_optim <-function(par_x,init_a,v)
{
  result <- optim(init_a, fn = square_error,x=par_x,v=v)
  return(result)
}


sub_interval <- function(original_function,numbers_of_interval)
{
  interval_point_list <- (0:numbers_of_interval)/numbers_of_interval
  value_interval <- c()
  for (i in 1:(length(interval_point_list)-1))
  { 
    start <- interval_point_list[i]
    end <- interval_point_list[i+1]
    mid <- (start+end)/2
    x <- c(start,mid,end)
    fn1 <- function(x) original_function(x)
    v <- fn1(x)
    res <- use_optim(x,c(1,1,1),v)
    a <- res[['par']]
    fitted <- c()
    for(i in x)
    {
      px <- c(0,i,i**2)
      fitted <- c(fitted,t(px)%*%a)
    }
    value_interval <- c(value_interval,fitted)
  }
  aex <- 1:(3*(length(interval_point_list)-1))/(3*(length(interval_point_list)-1))
  
  plot(aex,value_interval)
  plot(aex,fn1(aex))
  
  df <- data.frame(x = aex,fitted=value_interval,origin=fn1(aex))
  # reshape data so that 2 plot lines can be plotted in a single graph
  df1 <- melt(df,id.vars='x')
  # plot
  p1 <-ggplot(df1,aes(x=x,y=value))+
    geom_point(aes(color=variable))+
    ggtitle('fitted condition')+
    theme(plot.title = ggplot2::element_text(hjust=0.5))
  print(p1)
}
f1 <- function(x)
{
  return(-x*(1-x))
}
f2 <- function(x)
{
  return(-x*sin(10*pi*x))
}
sub_interval(f2,200)
################################################################################
# Question 2 
load('data.RData')
log_likelihood <- function(data)
{
  n <- length(data)
  miu <- sum(data)/n
  sigma <- sqrt(1/n*sum((data-miu)**2))
  return(c(miu,sigma))
}
minus_log_likelihood <- function(data,par)
{ 
  n <- length(data)
  first_part <- -n*log(1/(par[2]*sqrt(2*pi)))
  second_part <- 1/2*sum(((data-par[1])/par[2])**2)
  return(first_part+second_part)
}

gr_function <- function(par,data){
  mu <- sum(par[1]-data)/par[2]**2
  sd <- -length(data)/par[2]-sum((data-par[1])**2)/(par[2]**3)
  return(c(mu,sd))
}

conjugate_gradient <- function(par,minus_log_likelihood,data)
{
  res <- optim(par,fn=minus_log_likelihood,data=data,method = 'CG')
  return(res)
}

conjugate_gradient_withgr <- function(par,minus_log_likelihood,data,gr)
{
  res <- optim(par,fn=minus_log_likelihood,data=data,method = 'CG',gr=gr)
  return(res)
}


BFGS <- function(par,minus_log_likelihood,data)
{
  res <- optim(par,fn=minus_log_likelihood,data=data,method = 'BFGS')
  return(res)
}

BFGS_withgr <- function(par,minus_log_likelihood,data,gr)
{
  res <- optim(par,fn=minus_log_likelihood,data=data,method = 'BFGS',gr=gr)
  return(res)
}

cg <- conjugate_gradient(c(0,1),minus_log_likelihood,data)
bf <- BFGS(c(0,1),minus_log_likelihood,data)
cg_gr <- conjugate_gradient_withgr(c(0,1),minus_log_likelihood,data,gr_function)
bf_gr <- BFGS_withgr(c(0,1),minus_log_likelihood,data,gr_function)












