#' @docType data
#' @name wiki_graph
#' @title wiki_graph
#' @references /url:https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @description (v1,v2,w)means a graph.For example,as seen in column 1 (1,2,7)
#'     means there is edge linking V1 and V2 whose length is 7,as well as last 
#'     column (6,5,9) means there is edge linking V5 and V6 whose length is 9.
#'
NULL
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
