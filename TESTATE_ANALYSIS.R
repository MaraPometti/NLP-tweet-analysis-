install.packages("rtweet")
library(rtweet)
library(dplyr)
library(tidytext)
library(readr)
library(tidyr)
install.packages("tm")
library(tm)
install.packages("stringr")
library(stringr)
install.packages("NLP")
library(NLP)
install.packages("ggplot2")
library(ggplot2)
install.packages("textdata")
install.packages("textclean")
library(textclean)
library(textdata)

gironalisti_3 <- quotidiani
colnames(gironalisti_3)

#clean up from retweet: just searching for original tweets
no_rts <- gironalisti_3[grep("^RT ", gironalisti_3$content, invert=TRUE),] 
no_rts['content']

# Get the text column
text <- no_rts$content

# Set the text to lowercase
text <- tolower(text)
head(text)

# Remove mentions, urls, emojis, numbers, punctuations, etc.
text <- gsub("@\\w+", "", text)
text <- gsub("https?://.+", "", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("#\\w+", "", text)
text <- gsub("[^\x01-\x7F]", "", text)
text <- gsub("[[:punct:]]", " ", text)
head(text)

# Remove spaces and newlines
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)
head(text)

# Put the data to a new column
no_rts["fix_text"] <- text
head(no_rts$fix_text, 10)

colnames(no_rts)

# Get the text column
text_tweet <- no_rts$fix_text
head(text_tweet)


# Convert text_tweet in "twt_gsub" dataset to a text corpus and view output
head(text_tweet)

twt_corpus <- text_tweet %>% 
  VectorSource() %>% 
  Corpus() 
head(twt_corpus$content)

# Remove Italian stop words from the corpus and view the corpus
twt_corpus_stpwd <- tm_map(twt_corpus, removeWords, stopwords("italian"))
head(twt_corpus_stpwd$content)

install.packages("qdap")
library(qdap)

# Extract term frequencies for top 60 words and view output
termfreq  <-  freq_terms(twt_corpus, 60)
termfreq

# Create a vector of custom stop words
custom_stopwds <- c("di", "con", "alle", "nella", "nei", "c", "sar", "pu", "dall", "primo", "nelle", "qui", "hanno", "hanno", "tutto", "ancora" , "tre", "sui", "o", "de", "perch", "perch", "usa", "senza", "o", "ad", "aggiornamento", "nuovo", "se", "sulla", "degli", "cos", "il", "e", "la", "in", "a", "per", "l", "i", "del", "un", "delle", "le", "non", "che", "al", "da", "della", "su", "si", "una", "oggi", "dei", "ma", "pi", "nel", "gli", "ha", "sono", "rep", "alla", "dell", "dal", "lo", "anche", "come", "ecco", "anni", "all", "ai", "tra", "sul", "ci", "dopo", "d", "due", "prima", "contro", "ora", "dalla", "via","fa", "chi", "solo","nelle", "tutti")

# Remove custom stop words and create a refined corpus
corp_refined <- tm_map(twt_corpus,removeWords, custom_stopwds) 

# Extract term frequencies for the top 20 words
termfreq_clean <- freq_terms(corp_refined, 30)
termfreq_clean

# write.csv(no_rts,"/Users/mara.pometti@ibm.com/Desktop/no_rts_clean.csv", row.names = TRUE)

# sw <- tibble(word = stopwords("it"))
# head(sw)

# Remove Stopwords
# stop_id <- sw

# Create dataframe of the stop words
#stop_words <- data.frame(
#word <- stop_id,
#stringsAsFactors = F
#)

#colnames(stop_words) <- "word"


#stopwords removing
#data_clean <- no_rts %>%
# unnest_tokens(word, fix_text) %>%
# anti_join(sw) %>%
# filter(word !="https" & word != "t.co" & word != "pi" & word != "dopo" & word != "d" & word != "rep" & word != "the" & word !="via" & word !="solo" & word !="to" & word !="of" & word !="me"& word != "cosa" & word != "poi") 

#colnames(data_clean)
#head(data_clean)

#write.csv(data_clean,"/Users/mara.pometti@ibm.com/Desktop/quotidiani_cleaned.csv", row.names = TRUE)

#names(data_clean)[names(data_clean) == "sentiment class"] <- "sentimento"

#data_clean_1 <- data_clean %>% group_by(word) %>% count(sort=TRUE)
#head(data_clean_1, 20)

## DATA MODELING ##

#define the type of source you want to use and how it shall be read
x <- VectorSource(data_clean$word)

#create a corpus object
x <- VCorpus(x)

# Create a document term matrix (DTM) from the pre-loaded corpus
dtm_quotidiani <- DocumentTermMatrix(data_clean$word)
dtm_quotidiani

# Find the sum of word counts in each document
rowTotals <- apply(dtm_quotidiani, 1, sum)
head(rowTotals)

# Select rows with a row total greater than zero
dtm_quotidiani_new <- dtm_quotidiani[rowTotals > 0, ]
dtm_quotidiani_new

install.packages('topicmodels')
library(topicmodels)

# Create a topic model with 5 topics
topicmodl_5 <- LDA(dtm_quotidiani_new, k = 10)

# Select and view the top 10 terms in the topic model
top_10terms <- terms(topicmodl_5,10)
top_10terms 
