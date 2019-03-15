library(tm)
library(qdap)
library(tibble)
library(ggplot2)
library(RWeka)
library(wordcloud)
library(lubridate)
library(lexicon)
library(tidytext)
library(lubridate)
library(stringr)
library(dplyr)
library(radarchart)
library(readtext)

df <- read.csv("immigration_tweets.csv")

df$text <- gsub("...$","", df$text)
df$text <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", df$text)
df$text <- str_replace_all(df$text, "'", "")
df$text <- str_replace_all(df$text, "'", "")
df$text <- str_replace_all(df$text, "'", "")
df$text <- str_replace_all(df$text, "'", "")
df$text <- str_replace_all(df$text, """, "")
df$text <- str_replace_all(df$text, """, "")
df$text <- str_replace_all(df$text, "'", "")
df$text <- str_replace_all(df$text, ".", "")  

df$text = gsub("&amp", "", df$text)
df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$text)
df$text = gsub("@\\w+", "", df$text)
df$text = gsub("[[:punct:]]", "", df$text)
df$text = gsub("[[:digit:]]", "", df$text)
df$text = gsub("http\\w+", "", df$text)
df$text = gsub("[ \t]{2,}", "", df$text)
df$text = gsub("^\\s+|\\s+$", "", df$text) 
df$text <- gsub('uf.* *', '', df$text)


#get rid of unnecessary spaces
df$text <- str_replace_all(df$text," "," ")
# Take out retweet header, there is only one
df$text <- str_replace(df$text,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
df$text <- str_replace_all(df$text,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
df$text <- str_replace_all(df$text,"@[a-z,A-Z]*","")  

tweet_corpus <- VCorpus(VectorSource(df$text))

clean_corpus <- function(corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("french"))
  custom_stop_words <- c("immigration", "RT", "retweet", "ufufufthe", "uufef", "ufdstop",
                         "uf", "uufef", "uaufef", "ufd", "the", "and", "amp", "going", "this", "if",
                         "uuuuimmigrationideastelevisionmultiplechoicetheres", "yeah")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}

cleaned_tweet_corpus <- clean_corpus(tweet_corpus)

tidy_mytext <- tidy(TermDocumentMatrix(cleaned_tweet_corpus))

bing_lex <- get_sentiments("bing")
mytext_bing <- inner_join(tidy_mytext, bing_lex, by = c("term" = "word"))
mytext_bing$sentiment_n <- ifelse(mytext_bing$sentiment=="negative", -1, 1)
mytext_bing$sentiment_score <- mytext_bing$count*mytext_bing$sentiment_n

aggdata <- aggregate(mytext_bing$sentiment_score, list(index = mytext_bing$document), sum)

sapply(aggdata,typeof)

aggdata$index <- as.numeric(aggdata$index)

# ggplot(aggdata, aes(index, x,fill = x > 0)) +
#   
#   geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
#   
#   labs(x = "Tweets", y="Sentiment Score", title="Sentiment...")



#################################################################################

################### COMPARISON AND COMMONALITY CLOUDS ###########################

#################################################################################



#Using the sentiment score for each tweet, divide the tweets into positive and
#negative. The words from these two sets of tweets are then used to find commonalities
#through the commonality cloud and differences through the comparison cloud.



# split the sentiment scores by positive and negative tweet values

positive <- aggdata[ which(aggdata$x>0),]
negative <- aggdata[ which(aggdata$x<0),]



# create an index column in mytext to be able to subset by tweet number

df$index <- c(1:8000)



# Subset the tweets by positive and negative sentiments from the score via index value

ptweets <- df[ positive$index,]
ntweets <- df[ negative$index,]

positive_tweets <- paste(unlist(ptweets$text),collapse="")
negative_text <- paste(unlist(ntweets$text),collapse="")

speech <- c(positive_tweets,negative_text)

# making a corpus of a vector source 

speech_corpus <- Corpus(VectorSource(speech))
cleaned_speech_corpus <- clean_corpus(speech_corpus)

# TDM/DTM
TDM_speech <- TermDocumentMatrix(cleaned_speech_corpus,
                                 control =list(wordLengths=c(0,Inf)))
TDM_speech_m <- as.matrix(TDM_speech)

########################### Commonality Cloud ###################################

#this code creates a word cloud based on words that are common among both the
#positive and the negative tweets

par(mar = rep(0, 4))
commonality.cloud(TDM_speech_m,colors=brewer.pal(8, "Dark2"), max.words=300,
                  random.order=FALSE, rot.per=.18, scale=c(6,1))


# below creates a word cloud based on the words that are different between the
# positive and negative tweets
# 
# TDM_speech <- TermDocumentMatrix(cleaned_speech_corpus)
# colnames(TDM_speech) <- c("Positive","Negative")
# TDM_speech_m <- as.matrix(TDM_speech)
# 
# par(mar = rep(0, 4))
# pal <- wesanderson::wes_palette("Darjeeling1", type="continuous", n=2)
# comparison.cloud(TDM_speech_m,colors=pal, random.order=FALSE,
#      title.bg.colors = "white", title.colors = "black")
# 


