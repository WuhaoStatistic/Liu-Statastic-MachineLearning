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

