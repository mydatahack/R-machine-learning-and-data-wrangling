
shinyServer(function(input, output){
    
  data(diamonds)
  
  output$plot <- renderPlotly({
    
    carat_val = input$carat
    cut_val = input$cut
    color_val = input$color
    clarity_val = input$clarity
    
    df <- subset(diamonds, cut==cut_val & color==color_val & clarity==clarity_val)
    
    model <- loess(price ~ carat, df)
    predicted_price <- predict(model, newdata=data.frame(carat=carat_val))[[1]]
    
    output$predicted <- renderText({paste("$",round(predicted_price))})
    
    plot <- ggplot(data=df, aes(x=carat, y=price, alpha=0.5)) + geom_point(color="red") + geom_smooth(method="loess", color='green', alpha=0.5, size=0.7, se=F) +
      geom_point(x=carat_val, y=predicted_price, shape=3, color='blue', fill='blue', size=2.5, alpha=0.5) + ylab("$")
    
    ggplotly(plot)
  })
  
  
})