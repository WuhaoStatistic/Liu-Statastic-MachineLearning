# Q2.1
Reduce <- function(fn,v)
{
  s <- 0
  for (i in 1:length(v)) 
  {
    if(v[i]!=1 && v[i]!=0)
      stop('v must be vector consisted by 0 and 1')
    s <- fn(s,v[i])  
  }
  return(s)
}

logarithmic <- function(v)
{
  if(length(v) < 32)
  {
    for (i in 1:(32-length(v))) 
    {
      v <- c(0,v)
    }
  }
  return((-1)^(v[1])*exp(Reduce(function(s,v){s*2+v},v[3:32])*(-1)^v[2]))
}
###########################################################
# Q2.2
select <- function(pool)
{
  population <- c()
  for (i in 1:length(pool)) 
  {
    population<- c(population,Reduce(function(s,r){s*2+r},pool[[i]]))  
  }
  population <- abs(population)
  choosen <- sort(population,index.return = T)[[2]][1]
  choosen <- c(choosen,sample(sort(population,index.return = T)[[2]][2:length(pool)],1))
  # now i is the length of pool, aka the index of worst individual
  bad <- sort(population,index.return = T)[[2]][i]
  x1 <- pool[[choosen[1]]]
  x2 <- pool[[choosen[2]]]
  index_bad <- bad
  return(list(first = x1,second=x2,index_bad = index_bad))
}
###########################################################
# here x1 and x2 are the gene(binary vector) of best 2 individuals
# mutpro is the probability of mutation(for every bit)
crossover <- function(x1,x2,mutpro)
{
  new <- c()
  for (i in 1:length(x1)) 
  {
    gene <- -1
    choose <- runif(1)
    if(choose>0.5)
      gene <- x1[i]
    else
      gene <- x2[i] 
    # mutate
    # if 1 then mutate to 0,if 0 then mutate to 1
    muta <- runif(1)
    if(muta >(1-mutpro))
     gene <- abs(gene-1)
    new <- c(new,gene)
  }
  return(new)
}

genetic <- function(pool,mutpro)
{
  for (i in 1:100) 
  {
    l <- select(pool)
    new <- crossover(l[[1]],l[[2]],mutpro)
    pool[[(l[[3]])]] <- new
  }
  return(pool)
}

###########################################################
ini_pool <-function(n=50)
{
  pool <- list()
  for (i in 1:n) 
  {
    sam <- rep(0,32)
    sam[20:32] <- sapply(sam[20:32],FUN=function(x)if(runif(1)>0.2)x<-1 else x<-0)
    pool[[i]] <- sam
  }
  return(pool)
}
seep<-function(pool)
{
  r <- c()
  for (i in pool) 
  {
    r <- c(r,Reduce(function(s,r){s*2+r},i))
  }
  return(r)
}
pool <- ini_pool(50)
p <- genetic(pool,0.5)
seep(pool)
seep(p)
pool <- sapply(pool,FUN=function(x)if(runif(1)>0.2)x<-1 else x<-0,simplify = FALSE)
pool
