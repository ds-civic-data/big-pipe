library(httr)
library(jsonlite)
library(xml2)
library(rvest)

NEWSAPI_KEY <- 

src <- get_sources(language = "en")

guardian_uk_id <- src$id[47]



endpoint <- "/v2/everything"
#the-economist

query_test1 <- list(q = "string cheese")
query_test2 <- list(q = "string cheese", sources = "the-guardian-uk", apiKey = NEWSAPI_KEY)

get_test <- GET(url = "https://newsapi.org", path = endpoint, query = query_test2)

content_test <- content(get_test)

test_url <- content_test[["articles"]][[5]][["url"]]

test_html <- read_html(test_url)

#.js-article__body works for filtering body of text in guardian uk articles
test_text <- test_html %>%
  html_node(".js-article__body") %>%
  html_text()

get_newsapi_url <- function(url, q, path, sources, apiKey){
  query <- list(q = q, sources = sources, apiKey = apiKey)
  urls <- GET(url = url, path = path, query = query)
  content(urls)
}

get_guardian_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".js-article__body") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

get_politico_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".story-text") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

get_breitbart_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".entry-content") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

get_foxnews_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".article-body") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

get_natreview_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".article-content") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

get_thehill_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".even") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

get_bbcnews_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node(".story-body__inner") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

get_amconvs_html_text <- function(i, url_list){
  article_url <- url_list[["articles"]][[i]][["url"]]
  text <- read_html(article_url) %>% 
    html_node("#article-inner") %>%
    html_text()
  publish.date <- url_list[["articles"]][[i]][["publishedAt"]]
  c(text, publish.date)
}

#example when our search is string cheese
lapply(1:length(content_test[["articles"]]), get_guardian_html_text, content_test)

get_newsapi_url(url = "https://newsapi.org", q = "string cheese", 
                path = "/v2/everything", sources = "the-guardian-uk", apiKey = NEWSAPI_KEY) 
