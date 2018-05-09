library(tm) 
library(stringr)

library(mdsr)
library(tidytext)

# for use with sentiment analysis 
bingSentiments <- get_sentiments(lexicon = "bing")
afinnSentiments <- get_sentiments(lexicon = "afinn")

# DESCRIPTION 
## gets all html string data from arg, returning a list : (html_corpus, extracted_data_only_identifiers)  
# ARGUMENTS 
## extracted_data := return-value from text-extractor 
# RETURN 
# a 2-list, a corpus containing html strings, and function's argument,
# reduced to only its metadata 
split_extracted_data <- function(extracted_data) { 
  html_corpus <- c() # initialize character array 
  for (i in 1:length(extracted_data)) { 
    x <- extracted_data[[i]] 
    html_corpus[i] <- extracted_data[[i]][[5]] 
    extracted_data[[i]] <- extracted_data[[i]][-5] # delete hefty html data from extracted data 
  } 
  # convert to corpus and remove stop words 
  html_corpus <- Corpus(VectorSource(html_corpus)) 
  
  # now return 2-list : (html_dat, extracted_data) 
  l <- list(html_corpus, extracted_data) 
  l 
}

# DESCRIPTION 
## helper function for cleaning corpus content, see it's main function 
## remove_extraneous 
# ARGUMENTS 
## corpus_content := an element from the corpus 
# RETURN 
## a cleaned version of the function's argument, a corpus element 
remove_extraneous_helper <- function(corpus_content) { 
  corpus_content <- str_replace_all(corpus_content, "\\n", " ")
  #corpus_content <- str_replace_all(corpus_content, "[1234567890!@#$%^&*()_=+{}|[];<>?,./\"]]", "")
  corpus_content <- str_replace_all(corpus_content, "[[:punct:]]", " ")
  corpus_content <- str_replace_all(corpus_content, "1234567890", " ")
  corpus_content
}

# DESCRIPTION 
## cleans up corpus by tasks such as removing punctuation 
# ARGUMENTS 
## corpuz := the first element from function : split_extracted_data 
# RETURN 
## cleaned corpus 
remove_extraneous <- function(corpuz) {
  # convert to lower-case 
  #corpuz <- tm_map(corpuz, content_transformer(tolower))
  # remove stop-words 
  corpuz <- tm_map(corpuz, removeWords, stopwords("english"))
  # remove extraneous : newline, punc, and (numbas <- ??)  
  
  corpuz <- tm_map(corpuz, content_transformer(remove_extraneous_helper)) 
  corpuz 
}

# DESCRIPTION 
## converting an html string into a dataframe 
# ARGUMENTS 
## html_string := a stringo 
# RETURN 
## dataframe 
tokenize <- function(html_string) { 
  data_frame(text = html_string) %>%
    unnest_tokens(word, text) 
} 

# DESCRIPTION: 
## to be used as function to lapply, returns the sentiment score of 
## some html string based on arguments rubric and scoreType 
# ARGUMENTS 
## i := index in html_strings 
## html_strings := vector of html_strings 
## rubric := 'afinn' or 'bing'
## scoreType := 'all', 'positive', 'negative' 
##     *NOTE : this spec. is for users to know 
##      positive mentions, negative mentions, and overall opinion. 
# RETURN 
## integer 
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

# DESCRIPTION 
## converting html metadata into a cleaned corpus 
# ARGUMENTS 
## metadata := return value from test_extractor 
# RETURN 
## corpus 
metadata_to_corpus <- function(metadata) { 
  q <- split_extracted_data(metadata) 
  q[[1]] <- remove_extraneous(q[[1]])
  q
} 

# DESCRIPTION 
## returns a a list of tibbles, each is a score corresponding to html of same index 
# ARGUMENTS 
## corpus := a corpus of html strings (see function : metadata_to_corpus)
## rubric := "bing", "afinn" 
## scoreType := "all", "positive, "negative" 
# RETURN 
## list of tibbles (see description)
corpus_to_sentiments <- function(corpus, rubric, scoreType) {
  #x <- content(corpus)
  x1 <- corpus[[1]]
  x <- sapply(1:length(x1), function(i, x){x[[i]]["content"]}, x = x1)
  scores <- unlist(lapply(1:length(x), get_sentiment_score_by_rubric, x, rubric, scoreType)) 
  list(scores, corpus[[2]])
}


# TEST
## personal note : y is kinda chunky, due to da code das kinda funky 
x <- metadata_to_corpus(test.text2)
y <- corpus_to_sentiments(x, "bing", "negative")


