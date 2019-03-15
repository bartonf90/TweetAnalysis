library(qdap)
library(wordcloud)
library(tm)
library(RWeka)
library(stringr)
library(lubridate)

df <- read.csv("immigration_tweets.csv")

#iconv(df$text, from="UTF-8",to="latin1", sub="")
df$text <- gsub("...$","", df$text)
df$text <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", df$text)
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

#get rid of unnecessary spaces
df$text <- str_replace_all(df$text," "," ")
# Get rid of URLs
#df$text <- str_replace_all(df$text, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# Take out retweet header, there is only one
df$text <- str_replace(df$text,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
df$text <- str_replace_all(df$text,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
df$text <- str_replace_all(df$text,"@[a-z,A-Z]*","")  

# df$created <- mdy_hm(df$created)
# df$created <- with_tz(df$created, "America/New_York")

df$Date <- as.Date(df$created)
df$Time <- format(as.POSIXct(df$created), format = "%H:%M:%S") 

df$replyToSID <- NULL
df$id <- NULL
df$replyToUID <- NULL
df$statusSource <- NULL
df$longitude <- NULL
df$latitude <- NULL

tweet_corpus <- VCorpus(VectorSource(df$text))
# print(tweet_corpus)

# content(tweet_corpus[[16]])

clean_corpus <- function(corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("french"))
  custom_stop_words <- c("immigration", "RT", "retweet", "ufufufthe", "uufef", 
                         "uf", "uufef", "uaufef")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}

cleaned_tweet_corpus <- clean_corpus(tweet_corpus)

# create a structure for tri-gram
tokenizer <- function(x)
  NGramTokenizer(x,Weka_control(min= 3,max=3))
trigram_tdm <- TermDocumentMatrix(cleaned_tweet_corpus,control = list(tokenize=tokenizer))
trigram_tdm_m <- as.matrix(trigram_tdm)

# Term Frequency
term_frequency <- rowSums(trigram_tdm_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)
term_frequency

############Word Cloud
# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=30,max.words=300,
          colors=brewer.pal(8, "Dark2"), scale=c(3,.4), 
          random.color = FALSE, random.order = FALSE, rot.per = .15)

