library(shiny)
ui <- fluidPage(
               
  mainPanel(
    plotOutput("timeseries"),
    tableOutput("sentiment"),
          ),
  
 sidebarLayout( 
   sidebarPanel(
    selectInput(inputId = 'sources', label = 'News Sources'),
    htmlOutput("selectUI"),
    textInput(inputId = 'news_search', label = 'Search News',width = "400px"),
    submitButton(text = "Update Page",icon = "refresh")
               )
             )
 
    #must have list of news sources, write the code for the timeseries plot connected to the inputID. Similarly, sentiment table output
  #  below will reactively change based on the dataset. Must code in a reactive for that server-side. 
  
  
  
  
)

server < - function(input, output) {
  
  searchResult<- reactive({
    subset(news_text, grep(pattern = input$news_search, x = news_text$text, ignore.case = T ))
  })
  output$selectUI <- reactive({ 
    sources %>%
      filter(sources %in% input$sources)
  })
  
  output$searchResults <- renderTable({ 
    searchResult[,1]
  })
  output$timeseries <- renderPlot({
    timeseries
  }) 
  output$sentiment <- renderTable({
    sentiment()
  })
  
}


shinyApp(ui = ui, server = server)