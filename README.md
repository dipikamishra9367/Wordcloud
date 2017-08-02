# Wordcloud
Text Mining of Demonetization Tweets
 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(SnowballC)
library(wordcloud)
library(qdap)
library(tm)
library(dplyr)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

hacker_news <- read.csv("../input/hacker_news_sample.csv",stringsAsFactors=F)

str(hacker_news)
summary(hacker_news)

hacker_news<- data.frame(hacker_news)

 hacker_news <- hacker_news[ lapply( hacker_news, function(x) sum(is.na(x)) / length(x) ) < 0.05 ]
hacker_news <- hacker_news  %>% filter(type == "story") 
txt_news <- hacker_news$text
print(txt_news)
txt_news <- gsub("[^[:alnum:]///' ]", "", txt_news)
#cleaning the text using a qdap function
qdap_clean <- function(y) {
 y<- replace_abbreviation(y)
  y<- replace_contraction(y)
 y <- replace_number(y)
  y<- replace_ordinal(y)
  y<- replace_ordinal(y)
   y<- replace_symbol(y)
   y<- tolower(y)
 return(y)
}

txt_news <- qdap_clean(txt_news)

text_corp <- VCorpus(VectorSource(txt_news))
#cleaning the corpus
clean_corpus <- function(corpus){
  
  corpus <- tm_map(corpus, removePunctuation)
 corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
 corpus <- tm_map(corpus, PlainTextDocument)
  corpus <-  tm_map(corpus, stripWhitespace)
  corpus  <- tm_map(corpus, stemDocument)
  corpus
  return(corpus)
}

text_corp <- clean_corpus(text_corp)


#library(RWeka)
#tokenizer <- function(x) 
#  NGramTokenizer(x, Weka_control(min = 2, max = 2))

text_corp <- VCorpus(VectorSource(text_corp))
text_tdm <- TermDocumentMatrix (text_corp)

text_m<- as.matrix(text_tdm )

text_freq <- rowSums(text_m)
text_freq


# Create term_frequency
term_frequency <- sort(text_freq ,decreasing = TRUE )

# Print the 5 most common terms
term_frequency[1:20]

# Find associations with fast paced
findAssocs(text_tdm ,"fast paced",0.2 )


wordcloud(names(text_freq), color = "BLUE",min.freq=10)

