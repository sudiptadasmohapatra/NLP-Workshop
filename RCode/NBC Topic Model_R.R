###NBC Health Tweet Data Topic Modeling####

library(data.table)

nbc <- read.csv(file="~/Desktop/NBCHealthData.csv", head=T, stringsAsFactors = FALSE)
ls()
str(nbc)

#Loading libraries “dplyr” and “tidytext” to tokenize  
#You can also use library "stringr" when working with text strings
#You can use library "tidyr" for data reshaping
library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)

#use the unnest_tokens function to get the words from the "Health_Tweets" column of pcd
#unnest_tokens also converts all words into lower case
tidy_text <- nbc %>% unnest_tokens(word, Health_Tweets) 
str(tidy_text)
head(tidy_text)

#Removing stopwords
tidy_text <- tidy_text %>% anti_join(stop_words)

#Sorting by frequency of tokens 
tidy_text %>% count(word, sort = TRUE)

#create own stopword dictionary
my_stopwords <- tibble(word=c(as.character(1:10), 
                              "http", "nbcnews.to", "on.today.com", "u.s.", "study"))

tidy_text <- tidy_text %>%
  anti_join(my_stopwords)

#Sorting by frequency of tokens again
tidy_text %>% count(word, sort = TRUE)

#Visualizing sorted tokens with min freq of 50
library(ggplot2)
tidy_text %>% 
  count(word, sort = TRUE) %>%
  filter( n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

library(wordcloud)
tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200, random.order = FALSE))

#Stemming the orginal data
library(SnowballC)
tidy_text <- nbc %>% unnest_tokens(word, Health_Tweets) %>% mutate(word = wordStem(word, language="english")) 

#Removing stopwords
tidy_text <- tidy_text %>% anti_join(stop_words)
tidy_text <- tidy_text %>%
  anti_join(my_stopwords)

#Visualizing sorted tokens with min freq of 50 after stemming & removing stopwords
tidy_text %>% 
  count(word, sort = TRUE) %>%
  filter( n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Visualization of wordcloud with min token frequency of 200

library(wordcloud)
tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200,random.order = FALSE))

#It seems stemming of data is not useful here. 
#Without stemming the data was better.

#Sentiment Analysis 
#Let’s do sentiment analysis to tag +ve and -ve words using inner join function

x <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
x

##color-coded word cloud based on sentiment
library(reshape2)
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 70)

#You can also use the nrc method to get an expanded sentiment group 
#for each word
y <- tidy_text %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
y

##color-coded word cloud based on sentiment

tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

########################################

#TOPIC MODELING

library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
library(NLP)
library(RColorBrewer)


nbc_corpus <- Corpus(VectorSource(nbc$Health_Tweets))

nbc_corpus <- tm_map(nbc_corpus, removeNumbers)
nbc_corpus <- tm_map(nbc_corpus, removePunctuation)
nbc_corpus <- tm_map(nbc_corpus, stripWhitespace)
nbc_corpus <- tm_map(nbc_corpus, tolower)
nbc_corpus <- tm_map(nbc_corpus, removeWords, stopwords("english"))  
nbc_corpus <- tm_map(nbc_corpus, stemDocument, language = "english")
dtm_nbc <-DocumentTermMatrix(nbc_corpus) 

dtm_nbc


#see the first 10 rows and first 20 columns from the document term matrix)
dtm_nbc <-as.matrix(dtm_nbc)
dtm_nbc[1:10, 1:20]

rowTotals <- apply(dtm_nbc , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm_nbc[rowTotals> 0, ] #remove all docs without words

#Run the LDA model
lda <- LDA(dtm.new, k = 4) # k is the number of topics to be found.

lda

library(tidytext)
lda_td <- tidy(lda)
lda_td

#Visualization to find the 10 terms that are most common within each topic
library(ggplot2)
library(dplyr)

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic,scales="free")+
  coord_flip() 