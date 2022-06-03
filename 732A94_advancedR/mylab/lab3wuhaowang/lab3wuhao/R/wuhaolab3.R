#'This is lab3 assignment from Wuhao Wang liuid:wuhwa469
#'@title euclidean
#'
#'@author Wuhao Wang
#'
#'@description function name:euclidean
#'             return the maximum common divisor of x1 and x2
#'
#'@details By testing numbers from 1 to sqrt(min(x1,x2)),find all 'common divisor
#'
#'    If one of the parameter is negative,it will stop by calling stop().
#'
#'    If 0 is applied,it will also stop by calling stop().
#'
#'    Reference [link url:](https://en.wikipedia.org/wiki/Euclidean_algorithm)
#'
#'@param x1,x2    both are numeric
#'
#'@examples
#'    x1<-4;x2<-6;euclidean(x1,x2)
#'
#'    x1<-8;x2<-1;euclidean(x1,x2)
#'
#'@export
euclidean<-function(x1,x2)
{
  if(!is.numeric(x1)|!is.numeric(x2))
    stop()
  if(x1==0|x2==0)
    stop()
  if(x1==1|x2==1)
    return (1)
  x1<-abs(x1)
  x2<-abs(x2)
  big<-0
  small<-0
  if(x1>=x2)
  {
    big<-x1
    small<-x2
  }
  else
  {
    big<-x2
    small<-x1
  }
  if(big%%small==0)
    return (small)
  common<-1
  while(1)
  {
    changed_flag <-FALSE
    for(i in 2:sqrt(big))
    {
      if(small%%i==0&&big%%i==0)
      {
        common<-common*i
        small<-small/i
        big<-big/i
        changed_flag<-TRUE
        break
      }
    }
    if(!changed_flag)
      break
  }
  return (common)
}
#'This is lab3 assignment from Wuhao Wang liuid:wuhwa469
#'@title dijkstra
#'
#'@author Wuhao Wang
#'
#'@description function name :dijkstra.This function take a data.frame and a
#'numeric variable as parameter.In data.frame,there should be 3 column named v1
#'v2,w separately.v1 and v2 and w make up a graph.
#'
#'For example:
#'     if v1[1] = 1, v2[1] = 3,w[1] =4,
#'that means there is a edge linking V1 and V2,length is 2
#'     if v1[3] = 5, v2[3] = 8,w[3] =10,
#'that means there is a edge linking V5 and V8,length is 10
#'
#'the function will return a vector which contains the shortest distance
#'from Vinit_node to each other points.(for something like v1 to v1 it's 0)
#'
#'@details Here recursive method is applied.First,form a matrix mat1,which
#'mat1[x,y] is the length of edge linking Vx and Vy.If no edge there,mat1[x,y]
#'will be filled by NA_real_.
#'
#'There is another tool function to do recursive procedure,this function will
#'test every possible solution one by one and record the minimum solution.By
#'the way,in case of loop(like 1->3->1),there is a vector called "path" to
#'record visited point in case of  repeating.
#'
#'reference [link url:](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)
#'@param graph   data.frame
#'@param init_node  numeric
#'
#'@export
dijkstra<-function(graph,init_node)
{
  if(!is.data.frame(graph))
    stop()
  if(length(graph)<3)
    stop()
  if(sum(c("v1","v2","w")==names(graph))!=3)
    stop()
  en <- length(graph[[1]])
  V<-unique(graph[[1]])
  if(!init_node%in%V)
    stop()
  number_of_point<-length(V)
  mat1<-build_map(graph,number_of_point)
  res<-c()
  for(i in 1:number_of_point)
    res<-c(res,find_nearst(mat1,init_node,i,0,c(init_node),NA_real_))
  return(res)
}

#   This function solve the graph into a matrix, the value of mat1[x,y]
# means the distance from vx to vy.
#   If there is no road between,the value would be NA_real_.
#------------------------------------------------------------------------
#parameter:
#   graph: passed from dijkstra, the original data.frame
#   number: the number of points in the graph
build_map<-function(graph,number)
{
  mat1<-matrix(NA_real_,number,number)
  for(i in 1:length(graph[[1]]))
  {
    x<-graph$v1[i]
    y<-graph$v2[i]
    mat1[x,y]<-graph$w[i]
    mat1[y,x]<-graph$w[i]
  }
  return(mat1)
}
# This function will return the nearest distance between two given points.
#-------------------------------------------------------------------------
#parameter:
#   mat1:built in build_map,mat1[x,y] is the distance between x and y,to see
# specific information,take a look at function text of build_map
find_nearst<-function(mat1,from,to,already,path,short)
{
  if(from==to)
  {
    if(is.na(short))
      return(already)
    else
      return(min(already,short))
  }
  for(i in 1:nrow(mat1))
  {
    if(is.na(mat1[from,i])|(i%in%path))
      next
    if(!is.na(short)&(already+mat1[from,i]<short))
      short<-find_nearst(mat1,i,to,already+mat1[from,i],c(path,i),short)
    else if(is.na(short))
    {
      short<-find_nearst(mat1,i,to,already+mat1[from,i],c(path,i),short)
    }
  }
  return(short)
}
