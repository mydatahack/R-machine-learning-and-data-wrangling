# Render Text Example
# ui.R
library(shiny)

shinyUI(fluidPage(
  titlePanel("Practicing Shiny UI"),
  sidebarLayout(
    sidebarPanel(
     selectInput("fruits", "Select the value", choice=c("Strawberry"="Strawberry", "banana"="banana", "apple"="apple"))
    ),
    mainPanel(
      textOutput("text")
    )
  )
))

# server.R
library(shiny)
shinyServer(function(input, output){
  output$text<- renderText({
    input$fruits
  })
})
