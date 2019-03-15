library(qdap)
library(wordcloud)
library(tm)
library(RWeka)
library(stringr)
library(lubridate)

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

# Take out retweet header, there is only one
df$text <- str_replace_all(df$text,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
df$text <- str_replace_all(df$text,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
df$text <- str_replace_all(df$text,"@[a-z,A-Z]*","")   
df$text <- str_replace_all(df$text,"'","") 
df$text <- str_replace_all(df$text,"'","") 
df$text <- str_replace_all(df$text,"'","")

tweet_corpus <- VCorpus(VectorSource(df$text))
# print(tweet_corpus)

# content(tweet_corpus[[16]])
clean_corpus <- function(corpus){
  cleaned_corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("french"))
  custom_stop_words <- c("immigration", "RT", "retweet", "ufufufthe", "uufef", 
                         "uf", "uufef", "uaufef", "immigrat", "ti", "yeah", "ea",
                         "im", "ba", "ufd", "htt")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}


cleaned_tweet_corpus <- clean_corpus(tweet_corpus)

#create a structure for bi-gram
tokenizer <- function(x)
  NGramTokenizer(x,Weka_control(min=2,max=2))
bigram_tdm <- TermDocumentMatrix(cleaned_tweet_corpus,control = list(tokenize=tokenizer))
bigram_tdm_m <- as.matrix(bigram_tdm)

# Term Frequency
term_frequency <- rowSums(bigram_tdm_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)

############Word Cloud
# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=75,max.words=250,scale=c(1.7,.4),
          colors=brewer.pal(4, "Dark2"), rot.per=.14,random.order=FALSE)

write.csv(word_freqs, "bi_gram_word_freqs.csv")
