library(shiny)
library(tidyverse)
source("shinyhelper.R")
ui <- fluidPage(
               
  mainPanel(
    plotOutput("timeseries"),
    tableOutput("sentiment"),
    plotOutput("news_comp")
          ),
  
 sidebarLayout( 
   sidebarPanel(
    selectInput(inputId = 'sources', label = 'News Sources', multiple = T ),
    htmlOutput("selectUI"),
    textInput(inputId = 'news_search', label = 'Search News',width = "400px"),
    submitButton(text = "Update Page",icon = "refresh")
               )
             )
  
)


server < - function(input, output) {
  
  searchResult<- reactive({
    input$news_search
  })
  sources <- reactive({
    input$sources
  })
  data <- reactive({
   paste( c(input$searchResult, input$sources))
  })
    
  output$news_comp <- renderPlot({
    data %>%
      ggplot( aes( x = source, y = sentiment)) + geom_col()
  })
  
  output$timeseries <- renderPlot({
    data %>%
      ggplot( aes( x = date, y = sentiment)) + geom_col() + theme_classic()
  }) 

  output$sentiment <- renderTable({
    data %>%
      group_by(source) %>%
      summarize(mean = mean(sentiment))
  })
  
}


shinyApp(ui = ui, server = server)