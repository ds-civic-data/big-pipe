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

#guardian-uk
get_guardianuk_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".js-article__body") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#politico
get_politico_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".story-text") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#breitbart
get_breitbart_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".entry-content") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#foxnews
get_foxnews_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".article-body") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#national review
get_natreview_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".article-content") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#the hill
get_thehill_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".even") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#bbc news
get_bbcnews_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".story-body__inner") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#The American Conservative
get_amconvs_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node("#article-inner") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#The Economist (this one might not work)
get_economist_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".blog-post__inner") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}