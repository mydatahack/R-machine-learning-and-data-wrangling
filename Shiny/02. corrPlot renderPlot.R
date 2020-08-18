# renderPlot Example
# ui.R

library(shiny)
shinyUI(fluidPage(
  titlePanel("Corralation Matrix with corrplot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("method", "Choose Method", c("circle"="circle", "square"="square", "ellipse"="ellipse", "number"="number", "shade"="shade","color"="color", "pie"="pie")),
      selectInput("textColor", "Choose Tile Text Color", c("red"="red", "black"="black", "green"="green", "blue"="blue")),
      sliderInput("tilt", "Choose Top Tile Text Angle", min=0, max=90, step=10, value=90),
      radioButtons("Coefficient", "Add Coefficient", c("Remove"="No", "Show"="black"), selected="Remove")
      ),
    mainPanel(
      plotOutput("corrplot")
    )
  )
)
)

# server.R

library(shiny)
shinyServer(function(input, output){
  library(corrplot)
  data(mtcars)
  mcor <- cor(mtcars)
  output$corrplot <- renderPlot({
    method <- input$method
    col <- input$textColor
    angle <- input$tilt
    coef <- input$Coefficient
    if(coef=="No"){
      corrplot(mcor, method=method, tl.col=col, tl.srt=angle)
    } else{
      corrplot(mcor, method=method, tl.col=col, tl.srt=angle, addCoef.col=coef)
    }
  })
})