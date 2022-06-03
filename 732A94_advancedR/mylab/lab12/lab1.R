name<-"WuHao Wang"
liuid<-"wuhwa469"
my_num_vector<-function()
{
    return (c(log10(11),cos(pi/5),exp(pi/3),(1173%%7)/19))
}

filter_my_vector<-function(x, leq)
{
    x[x>=leq]<-NA
    return(x)
}

dot_prod<-function(a, b)
{
    return(sum(a*b))
}

approx_e<-function(N)
{
    sum <-0
    for(i in 0:N)
    {
        sum <- sum+1/factorial(i)  
    }
    return(sum)
}
my_magic_matrix<-function()
{
    return(matrix(c(4,9,2,3,5,7,8,1,6),3,3,byrow=TRUE)) 
}

calculate_elements<-function(A)
{
    return(length(A))
}

row_to_zero<-function(A, i)
{
    A[i,]<-0
    return(A)
}

add_elements_to_matrix<-function(A, x, i, j)
{
  A[i,j]<-A[i,j]+x
  return(A)
}

my_magic_list<-function()
{
  vec = my_num_vector()
  mt = my_magic_matrix()
  txt = "my own list"
  list1<-list(info = txt,vec,mt)
  return(list1)
}

change_info<-function(x, text)
{
  x$info<-c(text)
  return(x)
}

add_note<-function(x, note)
{
  x$note<-c(note)
  return(x)
}

sum_numeric_parts<-function(x)
{
  sum1 = 0
  for(i in x)
  {
    if(is.numeric(i))
      sum1<-sum(i)+sum1
  }
  return (sum1)
}

my_data.frame<-function()
{
  mdf<-data.frame(id=c(1,2,3),name=c('John','Lisa','Azra'),income=c(7.30,0.00,15.21),rich=c(FALSE,FALSE,TRUE))
  return(mdf)
}

sort_head<-function(df, var.name, n)
{
  newdf<-df[order(df[,var.name],decreasing = TRUE),]
  return(newdf[1:n,])
}

add_median_variable<-function(df, j)
{
  median1<-median(df[[j]])
  compared_to_median<-c()
  sm<-which(df[[j]] - median1 < -exp(-30))
  compared_to_median[sm]<-"Smaller"
  me<-which(abs(df[[j]] - median1) < exp(-30) )
  compared_to_median[me]<-"Median"
  gr<-which(df[[j]] - median1 > exp(-30))
  compared_to_median[gr]<-"Greater"
  newdf<-cbind(df,compared_to_median)
  return(newdf)
}
#head(add_median_variable(df = faithful, 1))

analyze_columns<-function(df, j)
{ 
  j1 = j[1]
  j2 = j[2]
  name_list<-c(names(df[j]),"correlation_matrix")
  name<-c("mean","median","sd")
  vec1<-c(mean(df[[j1]]),median(df[[j1]]),sd(df[[j1]]))
  vec2<-c(mean(df[[j2]]),median(df[[j2]]),sd(df[[j2]]))
  names(vec1)<-name
  names(vec2)<-name
  list1<-list(vec1,vec2,correlation_matrix = matrix(c(1,1,1,1),2,2))
  names(list1)<-name_list
  corNum<-cor(df[[j1]],df[[j2]])
  list1$correlation_matrix<-matrix(c(1,corNum,corNum,1),2,2)
  rownames(list1[[3]])<-c(names(df[j]))
  colnames(list1[[3]])<-c(names(df[j]))
  return(list1)
}



