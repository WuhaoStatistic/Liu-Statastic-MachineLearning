#' lab6 assignment
#'
#' @import Rcpp
#' @useDynLib LAB6
#' @importFrom Rcpp sourceCpp
#' brute_force_knapsack
#' @description brute_force_knapsack for lab6
#' @name
#' brute_force_knapsack
#' @export brute_force_knapsack
#' @export dynamic_programming
#' @export greedy_knapsack
#' @param x ,a data.frame. x$w is the weight,x$v is the value
#' @param W ,a integer. The maximum weight of items in the bag.
#'
#' @export
#' @return list , returns a list containing the maximum value and the index of items in x.
brute_force_knapsack<-function(x,W)
{
  if(!is.data.frame(x)|!(is.numeric(W)&W>=0))
    stop()
  length <- length(x[[1]])
  choose = sample(0,size = length, replace = TRUE)
  bestpick <- sample(0,size = length, replace = TRUE)
  max_value <- 0
  while(0 %in% choose)
  {
    choose <- vector_step(choose,length)
    picked_weight <- sum(choose*x$w)
    if(picked_weight <= W)
    {
      current_sum <- sum(choose*x$v)
      if(current_sum > max_value)
      {
        max_value <- current_sum
        bestpick  <- choose
      }
    }
  }
  if(sum(choose*x$w)<W)
    return(list(value = sum(x$v),elements = 1:length))
  else
  {
    value <- round(sum(bestpick*x$v)) 
    z<-seq(1,length)
    return(list(value=value,elements = z[bestpick == 1]))
  }
}

vector_step<-function(x1,length)
{
  carry_flag <- 1
  current_position <- length
  while(carry_flag==1&current_position > 0)
  {
    x1[current_position] <- x1[current_position] + 1
    if(x1[current_position]==2)
    {
      x1[current_position] <- 0
      current_position <- current_position -1
    }
    else
    {    
      carry_flag <- 0
    }
  }
  return(x1)
}
#' dynamic_programming
#' @description dynamic_programming for lab6
#' @name
#' dynamic_programming
#'
#' @param x ,a data.frame. x$w is the weight,x$v is the value
#' @param W ,a integer. The maximum weight of items in the bag.
#' @param fast, a logical variable. When it is set TRUE,then it will call Rcpp to solve.
#' @export
#' @return list , returns a list containing the maximum value and the index of items in x.
dynamic_programming<-function(x,W,fast = FALSE)
{ 
  if(!is.data.frame(x)|!(is.numeric(W)&W>=0))
    stop()
  if(!is.logical(fast))
    stop()
  if(fast == FALSE){
    length <- length(x[[1]])
    dp <- matrix(0,length+1,W+1)
    dp[0,] <- 0
    dp[,0] <- 0
    for(i in 1:length)
    {
      for(j in 1:W)
      {
        if(i==1L)
        {  
          if(j >= x$w[1])
          {
            dp[2,j+1] <- x$v[1]
            next
          }
        }
        else if(x$w[i] >j)
        {
          dp[i+1,j+1] <- dp[i,j+1]
          next
        }
        else
          dp[i+1,j+1] <- max(dp[i,j+1],dp[i,j-x$w[i]+1]+x$v[i])
      }
    }
    value <-dp[length+1,W+1]
    j <- W
    inpackage <- c()
    for(i in length:1)
    { 
      if(dp[i+1,j+1][1] == dp[i,j-x$w[i]+1][1]+x$v[i])
      {
        j <- j-x$w[i]
        inpackage <- c(inpackage,i)
      }
    }
    res <- list(value = round(value),elements = inpackage)
    return(res)
  }
  else
  {
    colnames(x)<-NULL
    x1<-as.matrix.data.frame(x,2,length)
    l1 <- FastDynamicProgramming(t(x1),W)
    value <- round(l1[length(l1)])
    elements <- l1[-length(l1)]
    res <- list(value=value,elements = elements)
    return(res)
  }
}
#' greedy_knapsack
#' @description greedy_knapsack for lab6,not very precise
#' @name
#' greedy_knapsack
#'
#' @param x ,a data.frame. x$w is the weight,x$v is the value
#' @param W ,a integer. The maximum weight of items in the bag.
#' @export
#' @return list , returns a list containing the maximum value and the index of items in x.
greedy_knapsack<-function(x, W)
{
  if(!is.data.frame(x)|!(is.numeric(W)&W>=0))
    stop()
  order_density <- order(x$v/x$w,decreasing = TRUE)
  j <- W
  value <- 0
  inpackage <- c()
  for(i in 1:length(x[[1]]))
  {
    if(j > x$w[order_density[i]])
    {
      j <- j - x$w[order_density[i]]
      value <- value + x$v[order_density[i]]
      inpackage <- c(inpackage,order_density[i])
    }
  }
  res <- list(value = value,elements = inpackage)
  return(res)
}