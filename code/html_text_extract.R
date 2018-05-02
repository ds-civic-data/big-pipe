library(httr)
library(jsonlite)
library(xml2)
library(rvest)

#functions in html_body_extract.R extract the 
#body of text from the output of get_newsapi_url
#given an index value. For use in lapply or map.
#Output is a list containing a list with two elements. First element is the 
#main body of text and the second element is the publication date as taken from
#publishedAt in output of get_newsapi_url

#example: output from example in url_retrieve.R of
#string cheese search on guardian-uk: guardian_strcheese
#lapply(1:length(guardian_strcheese[["articles"]]), get_guardianuk_html_text, guardian_strcheese)


news_id <- c("the-guardian-au", "the-guardian-uk", "politco", 
             "breitbart-news",  "fox-news", "national-review",
             "the-hill",  "bbc-news",  "the-american-conservative", "the-huffington-post", 
             "usa-today", "buzzfeed", "cnn", "al-jazeera-english", "cbs-news", "abc-news") 

html_sel <- c(".js-article__body",  ".js-article__body", ".story-text", ".entry-content", ".article-body", 
              ".article-content", ".even",".story-body__inner", "#article-inner", "#entry-text", 
              ".news-theme-color", ".xs-relative", ".l-container", "#body-200771816342556199", 
              "#article-entry", ".article-copy")

html.sel.df <- data.frame(news_id, html_sel)

html_text_extract <- function(i, url.list, html.sel.df){
  source_id <- url.list[[i]][2]
  index <- which(html.sel.df$news_id == source_id)
  source_html_code <- as.character(html.sel.df$html_sel[index])
  source_url <- url.list[[i]][4]
  text <- read_html(source_url) %>% 
    html_node(source_html_code) %>%
    html_text()
  url.list[[i]][5] <- text
  url.list[[i]]
}

index_na_entries <- function(i, text.list){
  if(anyNA(text.list[[i]]) == TRUE){
    i 
  }
}

text_extractor <- function(url.list){
  news_id <- c("the-guardian-au", "the-guardian-uk", "politco", 
               "breitbart-news",  "fox-news", "national-review",
               "the-hill",  "bbc-news",  "the-american-conservative", "the-huffington-post", 
               "usa-today", "buzzfeed", "cnn", "al-jazeera-english", "cbs-news", "abc-news") 
  
  html_sel <- c(".js-article__body",  ".js-article__body", ".story-text", ".entry-content", ".article-body", 
                ".article-content", ".even",".story-body__inner", "#article-inner", "#entry-text", 
                ".news-theme-color", ".xs-relative", ".l-container", "#body-200771816342556199", 
                "#article-entry", ".article-copy")
  
  html.sel.df <- data.frame(news_id, html_sel)
  
  text.list <- lapply(1:length(url.list), html_text_extract, 
         url.list = url.list, html.sel.df = html.sel.df)
  na_vec <- unlist(lapply(1:length(text.list), index_na_entries, text.list = text.list))
  text.list <- text.list[-na_vec]
  text.list
}

test.text <- text_extractor(test.news.url3)
