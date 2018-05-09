library(shiny)
library(tidyverse)
source("shinyhelper.R")
source("url_retirieve.R")
source("html_text_extract.R")
ui <- fluidPage(
  titlePanel("News Website Sentiment Analysis"),
   #could change mainpanel to fluidrow() and use columns to make site. Column widths always add up to 12.            
  mainPanel(
    plotOutput("timeseries"),
    tableOutput("sentiment"),
    plotOutput("news_comp")
          ),
  
 sidebarLayout( 
   sidebarPanel(
    selectInput(inputId = 'sources', label = 'News Sources', multiple = T ),
    htmlOutput("selectUI"),
   selectInput(inputId = 'searchterm', label = 'Search Term', multiple = F),
    submitButton(text = "Update Page",icon = "refresh")
               )
             )
  
)


server < - function(input, output) {
  
 
  sources <- reactive({
    input$sources
  })
  data <- reactive({
   paste( c(input$searchResult, input$sources))
  })
  
  searchterm <- reactive ({
    input$searchterm
  })
    
  output$news_comp <- renderPlot({
    data %>%
      filter(sources %in% input$sources, searchterm == input$searchterm) %>%
      ggplot( aes( x = source, y = sentiment)) + geom_col() + coord_flip() + theme_classic()
  })
  
  output$timeseries <- renderPlot({
    data %>%
      filter(sources %in% input$sources, searchterm == input$searchterm) %>%
      ggplot( aes( x = date, y = sentiment, lty = sources)) + geom_line() + theme_classic()
  }) 

  output$sentiment <- renderTable({
    data %>%
      group_by(source) %>%
      summarize(mean = mean(sentiment), median = median(sentiment), n = n(), max = max(sentiment))
  })
  
}


shinyApp(ui = ui, server = server)