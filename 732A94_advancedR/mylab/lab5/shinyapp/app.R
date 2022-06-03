library(shiny)
library(lab5class)
ui <- fluidPage(
  titlePanel("List of Cities in Several Countries"),
  "available list:Denmark,Sweden,Finland,Norway,Grinland.(Upper initial)",
  sidebarPanel(
    textInput(inputId = "addr",
              label = "Enter the country",
              value = "Finland"
    )
  ),
  mainPanel(
    tableOutput("mytable")
                )
  )
server <- function(input, output) 
{   wrong_flag<-0
  output$mytable <- renderTable({
    addr <- input$addr
    input_list <- c("Denmark","Sweden","Finland","Norway","Grinland")
    loca<-which(input_list==addr)
    if(!setequal(loca,integer(0)))
    {
      corre_list <- c('dk-7','se-7','fi-8','no-7','gl-7')
      a<- lab5class$new(corre_list[loca])
      result <- a$getdata()
      p <- length(result[[1]])%/%6 + 1
      x <- 6*p - length(result[[2]])
      for(i in 1:x)
      {
        result<-rbind(result,c(1,''))
      }
      lr <- data.frame(matrix(unlist(result[[2]]), nrow=p, byrow=T),stringsAsFactors=FALSE)
      lr
    }
    else
    {
     lr<-matrix("wrong Country name?",1,1)
     lr
    }
  })
}
shinyApp(ui = ui, server = server)