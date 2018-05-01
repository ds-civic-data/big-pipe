library(httr)
library(jsonlite)
library(xml2)
library(rvest)

NEWSAPI_KEY <- # insert your api here

src <- get_sources(language = "en")

guardian_uk_id <- src$id[47]

get_ex <- GET("https://newsapi.org/v2/everything?sources=the-guardian-uk&apiKey=dd82267324ef47b0aa8e7062eb1b5108")

endpoint <- "/v2/everything"
#the-economist

query_test1 <- list(q = "string cheese")
query_test2 <- list(q = "string cheese", sources = "the-guardian-uk", apiKey = NEWSAPI_KEY)

get_test <- GET(url = "https://newsapi.org", path = endpoint, query = query_test2)

content_test <- content(get_test)

test_url <- content_test[["articles"]][[5]][["url"]]

test_html <- read_html(test_url)

#.js-article__body works for filtering body of text in guardian uk articles
test_html %>%
  html_node(".js-article__body") %>%
  html_text()
