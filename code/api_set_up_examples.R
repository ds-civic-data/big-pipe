library(httr)
library(jsonlite)
library(xml2)
library(rvest)

library(mdsr)
library(tidytext)
library(newsAPI)

source("code/html_body_extract.R")
source("code/html_text_extract.R")
source("code/url_retrieve.R")

NEWSAPI_KEY <- 

src <- get_sources(language = "en")

guardian_uk_id <- src$id[47]


bingSentiments <- get_sentiments(lexicon = "bing")
afinnSentiments <- get_sentiments(lexicon = "afinn")


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


# NEW ADDITIONS : 5/2/2018 @ 7:15AM 
# START OF COMPUTATIONAL METHODS 

## to be used with lapply 
## strip all newlines, punctuation, convert to lowercase
## returns dataframe of tokenized html text 
get_html_tokens <- function(i, htmlList) { 
  targetArticle <- htmlList[i][[1]][1] 
  targetArticle <- str_replace_all(targetArticle, "\n", "")
  targetArticle <- str_replace_all(targetArticle, "[[:punct:]]", "")
  targetArticle <- tolower(targetArticle)
  
  tokens <- data_frame(text = targetArticle) %>% unnest_tokens(word, text) 
  return(tokens)
}

## to be used with lapply 
## gets the score of each tokenized html based on afinn or bing rubric (both of these sentiments have already been declared up top) 
## rubric := "afinn", "bing" 
## scoreType := "all", "positive", "negative" 

## CODER'S EXPLANATION : 
# I was thinking that scoreType can be used as a user-defined argument in the UI. 
# Let's say a user wants to search "some political event". Then they could specify "positive" for a ranking of 
# articles that mention the search positively, "negative" for mentioning the search negatively, and "all" for 
# overall opinion of the new's articles writer on the topic. 
# *This specification is a reminder of our objective : to display the sentiments of X news outlets on the same event/search
# * We should also code up a ranking method of news outlets sentiments in descending order. 
# * These ranked lists can be simply displayed as lists to the user in the UI. 
get_score_by_rubric <- function(i, htmlTokens, rubric, scoreType) { 
  targetTokens <- htmlTokens[[i]] 
  x <- ""
  # NOTE : afinn and bing sentiments have already been declared : do NOT want redeclaration 
  if (rubric == "afinn") { 
    x <- afinnSentiments
  } else { 
    x <- bingSentiments 
  }
  
  # join by sentimental words 
  joinedTokens <- targetTokens %>% 
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
            count()
            
    negTok <- joinedTokens %>% 
            filter(sentiment == "negative") %>% 
            count() 
    
    if (scoreType == "all") { 
      return(posTok + negTok)  
    } else if (scoreType == "positive") { 
      return(posTok)  
    } else { 
      return(negTok)  
    }
  }
}


#example when our search is string cheese
lapply(1:length(content_test[["articles"]]), get_guardian_html_text, content_test)

get_newsapi_url(url = "https://newsapi.org", q = "Kim Jong Un", 
                path = "/v2/everything", sources = "the-guardian-uk", apiKey = NEWSAPI_KEY) 

# my addition 
x <- lapply(1:length(content_test[["articles"]]), get_guardian_html_text, content_test)
tokens <- lapply(1:length(x), get_html_tokens, x) 
scores <- lapply(1:length(tokens), get_score_by_rubric, tokens, "bing", "all") 
  