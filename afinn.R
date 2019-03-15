library(qdap)
library(readtext)
library(rvest)
library(tm)
library(wordcloud)
library(RWeka)
library(tidytext)
library(dplyr)
library(tidyr)
library(radarchart)
library(ggplot2)
library(ggthemes)
library(quanteda)
library(waffle)
library(readtext)
library(extrafont)

mytext <- read.csv(file ="immigration_tweets3.csv")

mytext$text <- iconv(mytext$text, from = "UTF-8", to = "ASCII", sub = "")

mytext_corpus <- VCorpus(VectorSource(mytext$text))

clean_corpus <- function(corpus){
  #this variable is a function that removes links that begin with https 
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(corpus, removeURL)
  
  #this converts all the words in the tweets to lower case alphabets
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  
  #this gets rid of all the punctuations like !,?.
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  
  #this gets rid of numeric digits in the tweets
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  
  #this gets rid of all the stopwords in the tweets like articles, pronouns
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  
  #this creates a list of custom stop words and gets rid of custom words which the user can input and is topic related
  #custom_stop_words <- c()
  
  #cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  
  #this gets rid of the white spaces in the doc
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  
  return(cleaned_corpus) #returns the cleaned doc
  
}

cleaned_mytext_corpus <- clean_corpus(mytext_corpus)

TDM_texts <- TermDocumentMatrix(cleaned_mytext_corpus)
TDM_texts_m <- as.matrix(TDM_texts)
TDM_mytext <- TermDocumentMatrix(cleaned_mytext_corpus)
mytext_tidy <- tidy(TDM_mytext)
#term_frequency <- rowSums(TDM_texts_m)
View(term_frequency)
# TDM_mytext <- TermDocumentMatrix(cleaned_mytext_corpus)
# mytext_tidy <- tidy(TDM_mytext)

afinn_lex <- get_sentiments("afinn")
mytext_afinn_lex <- inner_join(mytext_tidy, afinn_lex, by = c("term" = "word"))
mytext_afinn_lex$sentiment_value <- mytext_afinn_lex$score * mytext_afinn_lex$count
mytext_afinn_lex$document <- as.numeric(rownames(mytext_afinn_lex))
afinn_aggdata1 <- aggregate(mytext_afinn_lex$sentiment_value, list(index = mytext_afinn_lex$document), sum)
afinn_aggdata1$index <- as.numeric(afinn_aggdata1$index)
colnames(afinn_aggdata1) <- c("index","afinn_score")

#### trying to create a diverging plot with ggplot2

random_afinn_lex <- mytext_afinn_lex %>% sample_n(20, replace = FALSE)
groupby_afinn_lex <- tapply(mytext_afinn_lex$sentiment_value, mytext_afinn_lex$term, FUN=sum)
groupby_afinn_lex
groupby_afinn_df <- data.frame(term = names(groupby_afinn_lex), score = groupby_afinn_lex)
afinn_ordered <- groupby_afinn_df[order(-groupby_afinn_df$score),]
afinn_top10 <- head(afinn_ordered, 5)
afinn_bottom10 <- tail(afinn_ordered, 5)
afinn_topbottom <- rbind(afinn_top10, afinn_bottom10) 




ggplot(afinn_topbottom, aes(x = term, y = score, fill = score > 0)) +
  geom_col()  + scale_fill_manual(values = c("TRUE" = "steelblue1", "FALSE" = "brown1")) +
  coord_flip() + labs(title="Sentiment Value by Term", y=" ", x="Term") +
  theme(text=element_text(size=12,  family="Consolas")) + guides(fill=FALSE) +
  theme_bw() +
  theme(plot.title=element_text(size=15, face="plain", 
                                family="Consolas",
                                color="black",
                                hjust=0.5,
                                lineheight=1.2),  # title
        axis.title.x=element_text(vjust=10,  
                                  size=12, family="Consolas",
                                  color="black"),  # X axis title
        axis.title.y=element_text(size=10, family="Courier New",
                                  color="black"),
        axis.text.x = element_text(family="Consolas",face="plain", color="black", 
                                   size=8),
        axis.text.y = element_text(family="Consolas",face="plain", color="black", 
                                   size=8)) 

