#' Coordinate finder
#'
#' The 'lab5class' using API from thenmap
#'
#' @field res A data.frame including id and name 
#' @field name_of_data Name of dataset you want 
#' @field availist Vector of available name of name_of_data
#' 
#' @import methods
#' @importFrom httr content GET
#' @importFrom rjson fromJSON
#'
#' @export lab5class
#' @exportClass lab5class
lab5class <- setRefClass("lab5class",
  fields = list
  (
    name_of_data = "character",
    res = "data.frame",
    availist = "vector"
 
  ),
  methods = list
  (
    initialize = function(Name)
    {
     availist <<-c('fi-8','ch-8','no-7','dk-7','se-7','se-4','us-4','gl-7') 
     if((!is.character(Name))||!Name%in%availist)
        stop("available dataset are fi-8, ch-8, no-7, no-4, dk-7, se-7, se-4, us-4, gl-7")
     name_of_data <<- Name
     res <<- data.frame()
     },
                      
     getdata = function()
     {
       url<- paste0("http://api.thenmap.net/v2/",name_of_data,"/data/*")
       get_url <- httr::GET(url = url)
       get_text <- httr::content(get_url,"text")
       json <- rjson::fromJSON(get_text)
       for(i in 1:length(json) )
       {
         res<<-rbind.data.frame(res,json[[i]])
       }
       return(res)
     }
  )  
)

