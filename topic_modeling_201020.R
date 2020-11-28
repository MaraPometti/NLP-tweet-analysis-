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
head(no_rts)

# Get the text column
#text_tweet <- no_rts$fix_text
#head(text_tweet)

install.packages("stopwords")
library(stopwords)
install.packages("SnowballC")
library(SnowballC)

# Remove Italian stop words from the corpus and view the corpus
# no_rts_second <- tm_map(no_rts, removeWords, stopwords("italian"))
# head(no_rts_second$fix_text)

sw <- tibble(word = stopwords("it"))
head(sw)


# Create a vector of custom stop words
custom_stopwds <- c("di", "con", "alle", "nella", "nei", "c", "sar", "pu", "dall", "primo", "nelle", "qui", "hanno", "hanno", "tutto", "ancora" , "tre", "sui", "o", "de", "perch", "perch", "usa", "senza", "o", "ad", "aggiornamento", "nuovo", "se", "sulla", "degli", "cos", "il", "e", "la", "in", "a", "per", "l", "i", "del", "un", "delle", "le", "non", "che", "al", "da", "della", "su", "si", "una", "oggi", "dei", "ma", "pi", "nel", "gli", "ha", "sono", "rep", "alla", "dell", "dal", "lo", "anche", "come", "ecco", "anni", "all", "ai", "tra", "sul", "ci", "dopo", "d", "due", "prima", "contro", "ora", "dalla", "via","fa", "chi", "solo","nelle","caterina","galloni","oroscopo","via","cosa","fare","stati","uniti","new","york", "tutti")


x  = no_rts$fix_text        #Company column data
x  =  removeWords(x,custom_stopwds)     #Remove stopwords

no_rts$fix_text_new <- x     #Add the list as new column and check

head(no_rts$fix_text_new, 10)

write.csv(no_rts,"/Users/mara.pometti@ibm.com/Desktop/no_rts.csv", row.names = TRUE)




#TOPIC MODELING

# Stem the tokens
tweet_tokens <- no_rts %>%
  unnest_tokens(output = "word", token = "words", input = fix_text_new) %>%
  anti_join(sw) %>%
  mutate(word = wordStem(word))

colnames(tweet_tokens)
  

# Create a document term matrix 
tweet_matrix <- tweet_tokens %>%
  count(id, word) %>%
  cast_dtm(document = id, term = word,
           value = n, weighting = tm::weightTf)

# Print the matrix details 
tweet_matrix

#less_sparse_matrix <-
#removeSparseTerms(tweet_matrix, sparse =.9999)

# Print results
#tweet_matrix
#less_sparse_matrix

install.packages("topicmodels")
library(topicmodels)

# Perform Topic Modeling
tweet_lda <-
  LDA(tweet_matrix, k = 10, method = 'Gibbs', control = list(seed = 1111))

# Extract the beta matrix 
tweet_betas <- tidy(tweet_lda, matrix = "beta")

# Topic #1
tweet_betas %>%
  filter(topic == 1) %>%
  arrange(-beta)


# Topic #2
tweet_betas %>%
  filter(topic == 2) %>%
  arrange(-beta)

# Topic #3
tweet_betas %>%
  filter(topic == 3) %>%
  arrange(-beta)

# Topic #5
tweet_betas %>%
  filter(topic == 5) %>%
  arrange(-beta)

# Topic #4
tweet_betas %>%
  filter(topic == 4) %>%
  arrange(-beta)

# Extract the beta and gamma matrices
sentence_betas <- tidy(tweet_lda, matrix = "beta")
sentence_gammas <- tidy(tweet_lda, matrix = "gamma")

# choose 15 words with highest beta from each topic
top_terms_5 <- sentence_betas %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

# plot the topic and words for easy interpretation
plot_topic_5 <- top_terms_5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

plot_topic_5



# Explore Topic 5
sentence_betas %>%
  filter(topic == 5) %>%
  arrange(-beta)

# Explore Topic 5 Gammas
sentence_gammas %>%
  filter(topic == 5) %>%
  arrange(-gamma)

# Extract the gamma matrix 
gamma_values <- tidy(tweet_lda, matrix = "gamma")

# Create grouped gamma tibble
grouped_gammas <- gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)

head(grouped_gammas)

# Count (tally) by topic
grouped_gammas %>% 
  tally(topic, sort=TRUE)

# Average topic weight for top topic for each sentence
grouped_gammas %>% 
  summarize(avg=mean(gamma)) %>%
  arrange(desc(avg))
