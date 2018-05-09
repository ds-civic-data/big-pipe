library(shiny)
library(tidyverse)

searchlist <- c("congress", "police", "metoo")
news_id <- c("the-guardian-au", "the-guardian-uk", "politico", 
             "breitbart-news",  "fox-news", "national-review",
             "the-hill",  "bbc-news",  "the-american-conservative", "the-huffington-post", 
             "usa-today", "buzzfeed", "cnn", "al-jazeera-english", "cbs-news", "abc-news") 


ui <- fluidPage(
  titlePanel("News Website Sentiment Analysis"),
   #could change mainpanel to fluidrow() and use columns to make site. Column widths always add up to 12.            
sidebarLayout(  
  
   sidebarPanel(
    selectInput(inputId = 'sources', label = 'News Sources', multiple = T, choices = news_id),
    htmlOutput("selectUI"),
   selectInput(inputId = 'searchterm', label = 'Search Term', multiple = F, choices = searchlist),
    submitButton(text = "refresh")
               ),
   mainPanel(
     tabsetPanel(
       tabPanel("Time-Series Plot", plotOutput("timeseries")),
       tabPanel("Comparison Plot", plotOutput("news_comp")),
       tabPanel("Sentiments Table", tableOutput("sentiment"))
     ))
 )
)


server <- function(input, output) {
  
 
 
  
  data <- reactive ({
    data <- proj.text.list[[input$searchterm]] %>%
      filter(news_source %in% input$sources)
  })
    
  output$news_comp <- renderPlot({
    data %>%
      ggplot( aes( x = source, y = sentiment)) + geom_col() + coord_flip() + theme_classic()
  })
  
  output$timeseries <- renderPlot({
    data %>%
      ggplot( aes( x = date, y = sentiment, lty = sources)) + geom_line() + theme_classic()
  }) 

  output$sentiment <- renderTable({
    data %>%
      group_by(source) %>%
      summarize(mean = mean(sentiment), median = median(sentiment), n = n(), max = max(sentiment))
  })
  
}


shinyApp(ui = ui, server = server)
