library(shiny)
ui <- fluidPage(
  headerPanel(
  plotOutput("timeseries")
               ),
  
  sidebarPanel(
    selectInput(inputId = 'sources', label = 'News Sources'),
    #must have list of news sources, write the code for the timeseries plot connected to the inputID. Similarly, sentiment table output
  #  below will reactively change based on the dataset. Must code in a reactive for that server-side. 
  
  tableOutput("sentiment")
             )
  
)

server < - function(input, output) {
  output$timeseries <- renderPlot({
    timeseries
  }) 
  output$sentiment <- renderTable({
    sentiment()
  })
  
}


shinyApp(ui = ui, server = server)