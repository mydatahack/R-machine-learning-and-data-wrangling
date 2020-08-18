

shinyUI(fluidPage(
  titlePanel("Diamond Price"),
  sidebarLayout(
    sidebarPanel(
      numericInput('carat', 'Enter Carat', min=0.1, max=5.0, step=0.1, value=1.0),
      selectInput('cut', 'Choose Cut', c('Ideal'='Ideal', "Premium"="Premium", "Very Good"="Very Good", "Good"="Good", "Fair"="Fair"),selected='Premium'),
      selectInput('color', 'Choose Color', c("J"="J", "I"="I", "H"="H", "G"="G", "F"="F", "E"="E", "D"="D"),selected='H'),
      selectInput('clarity', 'Choose Clarity', c("IF"="IF", "vvS1"="vvS1", "vvs2"="vvs2", "vs1"="vs1", "vs2"="vs2", "SI1"="SI1", "SI2"="SI2", "I1"="I1"),selected='VS1')
    ),
    mainPanel(
      h2("Predicted Diamond Price"),
      textOutput("predicted"),
      h2("Plot Output"),
      plotlyOutput('plot')
    )
  )
))