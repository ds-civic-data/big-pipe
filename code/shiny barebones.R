library(shiny)
library(tidyverse)
library(mdsr)
library(tidytext)
library(tm) 
library(lubridate)

load(file = "shiny_data/shiny.corpus.list.Rdata")
searchlist <- c("congress", "police", "metoo")

news_id <- c("the-guardian-au", "the-guardian-uk", "politico", 
             "breitbart-news",  "fox-news", "national-review",
             "the-hill",  "bbc-news",  "the-american-conservative", "the-huffington-post", 
             "usa-today", "buzzfeed", "cnn", "al-jazeera-english", "cbs-news", "abc-news") 

sentiment_choices <- c("bing", "afinn")

score_choices <- c("all", "positive", "negative")

measure_choices <- c("mean", "median")

bingSentiments <- get_sentiments(lexicon = "bing")
afinnSentiments <- get_sentiments(lexicon = "afinn")

tokenize <- function(html_string) { 
  data_frame(text = html_string) %>%
    unnest_tokens(word, text) 
} 

get_sentiment_score_by_rubric <- function(i, html_strings, rubric, scoreType) { 
  html_df <- tokenize(html_strings[i]) 
  
  if (rubric == "afinn") { 
    x <- afinnSentiments
  } else { 
    x <- bingSentiments 
  }
  
  joinedTokens <- html_df %>% 
    inner_join(x, by = "word")#[[1]]
  
  # return the score based on argument "scoreType"
  if (rubric == "afinn") {
    if (scoreType == "all") { 
      return(sum(joinedTokens$score))  
    } else if (scoreType == "positive") { 
      joinedTokens <- joinedTokens %>% 
        filter(score > 0) 
    } else { 
      joinedTokens <- joinedTokens %>% 
        filter(score > 0)  
    }
    return(sum(joinedTokens$score))
  } 
  
  if (rubric == "bing") { 
    posTok <- joinedTokens %>% 
      filter(sentiment == "positive") %>% 
      summarise(n = n()) 
    
    negTok <- joinedTokens %>% 
      filter(sentiment == "negative") %>% 
      summarise(n = -n()) 
    
    if (scoreType == "all") { 
      return(posTok + negTok)  
    } else if (scoreType == "positive") { 
      return(posTok)  
    } else { 
      return(negTok)  
    }
  }
}


## corpus := a corpus of html strings (see function : metadata_to_corpus)
## rubric := "bing", "afinn" 
## scoreType := "all", "positive, "negative"
corpus_to_sentiments <- function(corpus, rubric, scoreType) {
  #x <- content(corpus)
  x1 <- corpus[[1]]
  x <- sapply(1:length(x1), function(i, x){x[[i]]["content"]}, x = x1)
  scores <- unlist(lapply(1:length(x), get_sentiment_score_by_rubric, x, rubric, scoreType)) 
  list(scores, corpus[[2]])
}


#clean_sentiment takes as input the output of corpus_to_sentiments
#and outputs a tidy dataframe for use in the shinyapp
clean_sentiment <- function(sentiment.list){
  nRow <- length(sentiment.list[[2]])
  tidy.sent.df <- data.frame(matrix(unlist(sentiment.list[[2]]), 
                                    nrow = nRow, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(tidy.sent.df) <- c("NewsSource", "source_name", "Date", "url")
  tidy.sent.df <- data.frame(tidy.sent.df, SentimentScore = sentiment.list[[1]])
  tidy.sent.df %>%
    mutate(Date = ymd(sub("T.*", "", Date)))
}


ui <- fluidPage(
  titlePanel("News Website Sentiment Analysis"),
   #could change mainpanel to fluidrow() and use columns to make site. Column widths always add up to 12.            
sidebarLayout(  
  
   sidebarPanel(
    selectInput(inputId = 'searchterm', label = 'Search Term', multiple = FALSE, choices = searchlist), 
    selectInput(inputId = 'sources', label = 'News Sources', multiple = TRUE, choices = news_id),
    selectInput(inputId = 'sentimentlexicon', label = "Sentiment Lexicon", 
                multiple = FALSE, choices = sentiment_choices),
    selectInput(inputId = 'scoretype', label = "Score Type", multiple = FALSE, choices = score_choices),
    selectInput(inputId = 'sentimentmeasure', label = "Sentiment Measure", multiple = FALSE, choices = measure_choices),
    htmlOutput("selectUI"),
    submitButton(text = "refresh")
               ),
   mainPanel(
     tabsetPanel(
       tabPanel("Time-Series Plot", plotOutput("timeseries")),
       tabPanel("Aggregated Bar Plot", plotOutput("news_comp"))
       #tabPanel("Sentiments Table", tableOutput("sentiment"))
     ))
 )
)


server <- function(input, output) {
  
 
 
  
  data <- reactive ({
    
    #corpus_to_sentiments
    data <- corpus.list[[input$searchterm]] %>%
      filter(news_source %in% input$sources)
  })
    
  output$news_comp <- renderPlot({
    data %>%
      ggplot( aes( x = source, y = sentiment)) + geom_col() + coord_flip() + theme_classic()
  })
  
  output$timeseries <- renderPlot({
    data %>%
      ggplot( aes( x = Date, y = sentiment, lty = sources)) + geom_line() + theme_classic()
  }) 

  #output$sentiment <- renderTable({
  #  data %>%
  #    group_by(source) %>%
  #    summarize(mean = mean(sentiment), median = median(sentiment), n = n(), max = max(sentiment))
  #})
  
}


shinyApp(ui = ui, server = server)
