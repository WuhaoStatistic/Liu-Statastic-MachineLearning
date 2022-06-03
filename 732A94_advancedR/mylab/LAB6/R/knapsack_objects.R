#' @docType data
#' @name knapsack_objects
#' @title knapsack_objects
#' @description data used in lab6. It is the same as shown in lab6.pdf
#'
NULL

RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion") 
n <- 2000
knapsack_objects <-
  data.frame( w=sample(1:4000, size = n, replace = TRUE),
              v=runif(n = n, 0, 10000) )
