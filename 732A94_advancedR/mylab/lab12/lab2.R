name<-"Wuhao Wang"
liuid<-"wuhwa469"
sheldon_game<-function(player1, player2)
{
    list<-c("scissor","paper","rock","lizard","spock")
    if(!player1%in%list|!player1%in%list)
      stop()
    c1<-c(0,2,1,2,1)
    c2<-c(1,0,2,1,2)
    c3<-c(2,1,0,2,1)
    c4<-c(1,2,1,0,2)
    c5<-c(2,1,2,1,0)
    x<-switch(player1,"scissor"=1,"paper"=2,"rock"=3,"lizard"=4,"spock"=5)
    y<-switch(player2,"scissor"=1,"paper"=2,"rock"=3,"lizard"=4,"spock"=5)
    df<-data.frame(c1,c2,c3,c4,c5)
    if(df[x,y] == 1)
        return("Player 1 wins!")
    if(df[x,y] == 2)
        return("Player 2 wins!")
    if(df[x,y] == 0)
        return("Draw!")
}

my_moving_median<-function(x,n,...)
{
    if(!is.numeric(x)||!is.numeric(n))
        stop()
    res<-c()
    for(i in 1:(length(x)-n))
    {   
        end <- i+n
        res[i]=median(c(x[i:end]),na.rm=...)
    }
    return(res)
}

for_mult_table<-function(from,to)
{
  if(!is.numeric(from)||!is.numeric(to))
    stop()
  vec<-from:to
  mat1<-vec%*%t(vec)
  rownames(mat1)<-vec
  colnames(mat1)<-vec
  return(mat1)
}


cor_matrix<-function(x)
{ #x is dataFrame
  namelist<-names(x)
  df<-apply(x,2,function(i){i-mean(i)})
  colNumber<-ncol(df)
  mat1<-as.matrix(df[1:colNumber,1:colNumber])
  for(i in 1:colNumber)
  {
    for(j in 1:i)
    {
      x = df[,namelist[i]]
      y = df[,namelist[j]]
      mat1[i,j] = (x%*%y)/((sqrt(sum(x^2)))*(sqrt(sum(y^2))))
      mat1[j,i] = mat1[i,j]
    }
  }
  row.names(mat1)<-NULL
  colnames(mat1)<-NULL
  return(mat1)
}

find_cumsum<-function(x, find_sum)
{
  if(!is.numeric(x)||!is.numeric(find_sum))
    stop()
  sum = 0
  i <- 1
  while(i <= length(x) && sum<find_sum)
  {
    sum<- sum + x[i]
    i <- i+1
  }
  return(sum)
}

while_mult_table<-function(from,to)
{
  i<-from
  name<-from:to
  start<-name*i
  i<-i+1
  start<-as.data.frame(start)
  while(i<=to)
  {
    newlist<-name*i
    i<-i+1
    start<-cbind(start,newlist)
  }
  rownames(start)<-name
  colnames(start)<-name
  start<-as.matrix(start)
  return(start)
}

trial_division_factorization<-function(x)
{ 
  list<-c()
  while(1)
  {
    i = 2
    find_flag<-FALSE
    while(i<=sqrt(x))
    {
      if (x%%i!=0)
        i<-i+1
      else
        {
          find_flag<-TRUE
          x<-x/i
          break
        }
    }
    if(find_flag==TRUE)
      list<-c(list,i)
    else
      return(c(list,x))
  }
}

repeat_find_cumsum<-function(x,find_sum)
{
  if(!is.numeric(x)||!is.numeric(find_sum))
    stop()
  sum<-0
  i<-1
  repeat
  { 
    sum<-sum+x[i]
    i<-i+1
    if(i > length(x) | sum>find_sum)
      break
  }
  return(sum)
}

repeat_my_moving_median<-function(x,n,...)
{
  if(!is.numeric(x)||!is.numeric(n))
    stop()
  list<-c()
  i<-1
  repeat
  {
    end<-i+n
    list<-c(list,median((x[i:end]),na.rm=...))
    i <- i+1
    if((i+n)>length(x))
      break
  }
  return(list)
}

in_environment<-function(env)
{
  return(ls(env))
}

cov<-function(X)
{
  if(!is.data.frame(X))
    stop()
  name<-names(X)
  y<-lapply(X,function(X){sd(X)/mean(X)})
  y<-as.numeric(unlist(y))
  names(y)<-name
  return(y)
}

moment<-function(i)
{
  if(!is.numeric(i))
    stop()
  m<-function(p)
  {
    return(mean(((p)-mean(p))^i))
  }
  return(m)
}

mcmc_counter_factory<-function(burnin,thin)
{
  if(burnin<0|thin<=0)
    stop()
  counter<<-1
  stored_number_count<<-0
  mcmccnt<-function()
  {
    counter<<-counter+1
    if(counter-burnin>0&(counter-burnin)%%thin==0)
    {
      stored_number_count <<- stored_number_count+1
      return(list(counter,TRUE,stored_number_count))
    }
    else
    {
      return(list(counter,FALSE,stored_number_count))
    }
  }
  return(mcmccnt)
}




