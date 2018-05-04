library(httr)
library(jsonlite)
library(xml2)
library(rvest)

#get_newsapi_url is a function which queries newsapi given sources (the news sources, which can be multiple, 
#so long as separated by a comma in a single string), pagesize, a newsapi.org
#api key, url (which is "https://newsapi.org"), search term q (default is empty string, 
#which returns articles from the past month), path which is the 
#endpoint that can be either "/v2/everything" or "/v2/top-headlines", sortBy (default is publishedAt, 
#other options are "popularity" and "relevancy", and pageSize which is default 20 and must be a non-zero 
#integer with a maximum value of 100.

NEWSAPI_KEY <- "no key needed!"
  
get_newsapi_url <- function(url = "https://newsapi.org", q = "", 
                            path, sortBy = "publishedAt", sources, apiKey, pageSize = 20){
  query.init <- list(q = q, sources = sources,
                     sortBy = sortBy, apiKey = apiKey, pageSize = pageSize)
  urls.init <- GET(url = url, path = path, query = query.init)
  content.list.init <- content(urls.init)
  num.results <- content.list.init[["totalResults"]]
  num.pages <- ceiling(num.results/pageSize)
  
  final.list <- list()
  for(i in 1:num.pages){
  query <- list(q = q, sources = sources,
       sortBy = sortBy, apiKey = apiKey, page = i, pageSize = pageSize)
  urls <- GET(url = url, path = path, query = query)
  content.list <- content(urls)
  meta.data.list <- lapply(1:length(content.list[["articles"]]), 
                           extract_metadata, content.list = content.list)
  final.list <- c(final.list, meta.data.list)
  }
  
  final.list
}

#extract_metadata is a function we use in get_newsapi_url to obtain the following metadata:
#source_name, source_id (how the source is listed in newsapi.org), publish_date, and article_url
extract_metadata <- function(i, content.list){
  article_url <- content.list[["articles"]][[i]][["url"]]
  publish_date <- content.list[["articles"]][[i]][["publishedAt"]]
  source_id <- content.list[["articles"]][[i]][["source"]][[1]]
  source_name <- content.list[["articles"]][[i]][["source"]][[2]]
  c(source_name, source_id, publish_date, article_url)
}

#Example:
#test.news.url <- get_newsapi_url(q = "Trump", sources = "the-guardian-uk, bbc-news", 
#                apiKey = NEWSAPI_KEY, path = "/v2/everything", pageSize = 100)

#test.news.url2 <- get_newsapi_url(q = "", sources = "the-guardian-uk, bbc-news", 
#                                                 apiKey = NEWSAPI_KEY, path = "/v2/everything", pageSize = 100)


test.news.url3 <- get_newsapi_url(q= "Cheese", sources = "the-guardian-uk, bbc-news", 
                                               apiKey = NEWSAPI_KEY, path = "/v2/everything", pageSize = 100)

