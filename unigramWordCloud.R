library(qdap)
library(wordcloud)
library(tm)
library(RWeka)
library(stringr)
library(lubridate)
library(RColorBrewer)

df <- read.csv("backup_immigration_tweets.csv")

iconv(df$text, from="UTF-8",to="latin1", sub="")
df$text <- gsub("...$","", df$text)
df$text <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", df$text)
df$text = gsub("&amp", "", df$text)
df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$text)
df$text = gsub("@\\w+", "", df$text)
df$text = gsub("[[:punct:]]", "", df$text)
df$text = gsub("[[:digit:]]", "", df$text)
df$text = gsub("http\\w+", "", df$text)
df$text = gsub("[ \t]{2,}", "", df$text)
df$text = gsub("^\\s+|\\s+$", "", df$text) 

#get rid of unnecessary spaces
df$text <- str_replace_all(df$text," "," ")
# Take out retweet header, there is only one
df$text <- str_replace_all(df$text,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
df$text <- str_replace_all(df$text,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
df$text <- str_replace_all(df$text,"@[a-z,A-Z]*","")   
df$text <- str_replace_all(df$text,"'","")


tweet_corpus <- VCorpus(VectorSource(df$text))

clean_corpus <- function(corpus){
  cleaned_corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("french"))
  custom_stop_words <- c("immigration", "RT", "retweet", "ufufufthe", "uufef", 
                         "uf", "uufef", "uaufef", "immigrat")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}

cleaned_tweet_corpus <- clean_corpus(tweet_corpus)
# cleaned_tweet_corpus <- gsub("[[:punct:]]", "", cleaned_tweet_corpus)
# print(cleaned_tweet_corpus[[16]][1])

# TDM_reviews <- TermDocumentMatrix(cleaned_tweet_corpus)
# TDM_reviews_m <- as.matrix(TDM_reviews)
# 
# term_frequency <- rowSums(TDM_reviews_m)
# # Sort term_frequency in descending order
# 
# 
# 
# mycols <- c("#665191","#a05195","#d45087","#f95d6a","#ff7c43", "#ffa600", "#2f4b7c")
# # Create word_freqs
# word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# # Create a wordcloud for the values in word_freqs
# # par(mar = rep(0, 4))
# wordcloud(word_freqs$term, word_freqs$num,min.freq=7,max.words=250, 
#           colors = mycols, scale=c(3,.7), random.order=FALSE, random.color = FALSE,
#           rot.per=.2)



# TF-IDF
tfidf_tdm <- TermDocumentMatrix(cleaned_tweet_corpus,control=list(weighting=weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)

# Term Frequency
term_frequency <- rowSums(tfidf_tdm_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)


###########Word Cloud
# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
#Create a wordcloud for the values in word_freqs
par(mar = rep(0, 4))
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,
          random.order = FALSE, max.words=3000,scale=c(4,.8),
          colors=brewer.pal(8, "Dark2"))







