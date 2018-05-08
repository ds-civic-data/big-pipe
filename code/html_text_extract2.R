## THIS FILE IS WHERE THE PARALLEL VERSIONS OF HTML_EXTRACT GOES 
## WORK IN PROGRESS 

# the process by which extraction takes place 

library(foreach)
library(iterators)

news_id <- c("the-guardian-au", "the-guardian-uk", "politico", 
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
  text <- tryCatch(
    read_html(source_url) %>% 
      html_node(source_html_code) %>%
      html_text(),
    error = function(cond) {
      return(NA)
    }
  )
  url.list[[i]][5] <- text
  url.list[[i]]
}

index_na_entries <- function(i, text.list){
  if(anyNA(text.list[[i]]) == TRUE){
    i 
  }
}

test.news.url4 <- get_newsapi_url(q= "Cheese", sources = "the-guardian-uk, bbc-news", 
                                  apiKey = NEWSAPI_KEY, path = "/v2/everything", pageSize = 100)


### SAMPLE SYNTAX 

#> rf <- foreach(ntree=rep(250, 4), .combine=combine, .packages=
#'
#randomForest
#'
#) %dopar%
#  +   randomForest(x, y, ntree=ntree)
####

text <- tryCatch(
  read_html(source_url) %>% 
    html_node(source_html_code) %>%
    html_text(),
  error = function(cond) {
    return(NA)
  }
)

# DESCRIPTION 
## implementation of html_extractor code using basic foreach 
# ARGUMENTS 
## metadata := output from function : get_newsapi_url 
## newDF := see above df : html.sel.df 
html_extract2 <- function(metadata, newDF) { 
  # add :    .combine = combine 
  start <- Sys.time()
  text_list <- foreach(i = 1:length(metadata)) %dopar% {
    source_id <- metadata[[i]][2]
    index <- which(newDF$news_id == source_id)
    source_html_code <- as.character(newDF$html_sel[index])
    source_url <- metadata[[i]][4]
    
    text <- tryCatch(
      read_html(source_url) %>% 
        html_node(source_html_code) %>%
        tml_text(),
      error = function(cond) {
        return(NA)
      }
    )
    
    metadata[[i]][5] <- text
    metadata[[i]]
  }
  
  na_vec <- unlist(lapply(1:length(text_list), index_na_entries, text.list = text_list))
  text_list <- text_list[-na_vec]
  
  runtime <- Sys.time() - start 
  print(runtime)
  text_list
}



################################### FLAWED REASONING 

# ARGUMENTS 
## newsSourceCol := element in vector : news_id 
## htmlSelector := element in vector : html_sel 
## metadata := return value from function : get_news_api_url 
# RETURN 
## list of elements corresponding to some news source 
html_extract3_helper <- function(newsSourceCol, htmlSelector, metadata) {
  foreach(i = 1:length(metadata)) %dopar% { 
    source_id <- metadata[[i]][2]
    if (source_id == newsSourceCol) { 
      source_html_code <- as.character(htmlSelector) 
      source_url <- metadata[[i]][4]
      
      text <- tryCatch(
        read_html(source_url) %>% 
          html_node(source_html_code) %>%
          html_text(),
        error = function(cond) {
          return(NA)
        }
      )
    }
  }
}
  

# DESCRIPTION 
## implementation of html_extractor code using extension of apply function  
# ARGUMENTS 
## metadata := output from function : get_newsapi_url 
## newDF := see above df : html.sel.df
html_extract3 <- function(metadata, newDF) { 
  start <- Sys.time()
  Q <- foreach(x = iter(newDF, by = "row"), .combine=list) %dopar% {
          html_extract3_helper(x$news_id, x$html_sel, metadata) 
  }
  print(Sys.time() - start)
  Q 
} 


testIter <- function(newDF) { 
  foreach(x = iter(newDF, by = "row")) %dopar% 
    print(x$news_id)
}



######## SAMPLE SYNTAX FOR PARALLELIZATION 
'
> applyKernel <- function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  +   foreach(i=1:d2) %dopar%
    +     FUN(array(newX[,i], d.call, dn.call), ...)
'
# > applyKernel(matrix(1:16, 4), mean, 4, 4)

###
'
> applyKernel <- function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  +   foreach(x=iter(newX, by= "col" 

  )) %dopar%
    +     FUN(array(x, d.call, dn.call), ...)
'
########

#testing <- html_extract2(test.news.url4, html.sel.df)

testing2 <- html_extract3(test.news.url4, html.sel.df)

#testIter(html.sel.df)