library(httr)
library(jsonlite)
library(xml2)
library(rvest)

#get_nesapi_url is a function which queries newsapi given a source and 
#newsapi apiKey. url is the url "https://newsapi.org", q is the search term,
#path is the endpoint which can be either "/v2/everything" or "/v2/top-headlines".


get_newsapi_url <- function(url = "https://newsapi.org", q, path, sources, apiKey){
  query <- list(q = q, sources = sources, apiKey = apiKey)
  urls <- GET(url = url, path = path, query = query)
  content(urls)
}

#Example:
#guardian_strcheese <- get_newsapi_url(q = "string cheese", sources = "the-guardian-uk", 
#                 path = "/v2/everything", apiKey = NEWSAPI_KEY)