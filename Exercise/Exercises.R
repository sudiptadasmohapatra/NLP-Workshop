
####EXERCISE 1 for STUDENTS

#1. Extract digits from a string of characters
exercise<- "My id number is 1006781"




#2. Remove punctuation from a line of text

exercise_2<- "a1~!@#$%^&*bcd(){}_+:efg\"<>?,./;'[]-="





#3. Extract email addresses from a given string
email <-c("My email address is sudipta.dasmohapatra@duke.edu", "my email address is sd@work.edu", "esther koeif", "paul donegg")






####EXERCISE 2 for STUDENTS
#Use TM package in R to construct a Word Cloud using Martin Luther King's 
#I have a dream speech Data=dream.txt

text <- readLines(file.choose())

#You can read the text document from the web as well as reading it from your desktop
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

# 1. Load the data as a corpus
docs <- Corpus(VectorSource(text))

#2. Inspect content of the document
inspect(docs)






#############################################################
#SOLUTIONS to EXERCISE 1

#1. Extract digits from a string of characters (any three work)
exercise<- "My id number is 1006781"

gsub(pattern="[^0-9]", replacement="", x=exercise)
regmatches(exercise, regexpr("[0-9]+", exercise))
regmatches(exercise, regexpr("[[:digit:]]+", exercise))


#2. Remove punctuation from a line of text

exercise_2<- "a1~!@#$%^&*bcd(){}_+:efg\"<>?,./;'[]-="
gsub(pattern="[[:punct:]]+", replacement="", x=exercise_2)


#3. Extract email addresses from a given string
email <-c("My email address is sudipta.dasmohapatra@duke.edu", "my email address is sd@work.edu", "esther koeif", "paul donegg")
unlist(regmatches(x=email, gregexpr(pattern = "[[:alnum:]]+\\@[[:alpha:]]+\\.edu", text=email)))


#############################################################
#SOLUTIONS to EXERCISE 2

# Load
library("tm")
library("NLP")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#The text is loaded using Corpus() function from text mining (tm) package. 
#Corpus is a list of a document (in our case, we only have one document).
#Import the text file created in Step 1- Choose the text file interactively.
#You can either choose the text file locally from your computer
text <- readLines(file.choose())

#You can read a text document from the web
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

# 1. Load the data as a corpus
docs <- Corpus(VectorSource(text))

#2. Inspect content of the document
inspect(docs)

#3. Transformation is performed using tm_map() function to replace, 
#for example, special characters from the text.
#Replacing “/”, “@” and “|” with space:

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#4. TEXT PREPROCESSING 
#a. WHITE SPACE: the tm_map() function is used to remove unnecessary white space, 
#b. STOPWORDS: to convert the text to lower case, to remove common stopwords like ‘the’, “we”.
#The information value of ‘stopwords’ is near zero due to the fact that they are common. 
#c. REMOVE NUMBERS AND PUNCTUATIONS: use removeNumbers and removePunctuation arguments.
#d. TEXT STEMMING :reduces words to their root form -removes suffixes from words
#Words such as “moving”, “moved” and “movement” change to root word, “move”

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

#4. BUILD A TERM DOCUMENT MATRIX
#Document matrix is a table containing the frequency of the words. 
#Column names are words and row names are documents. 
#The function TermDocumentMatrix() from text mining package can be used

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#5. GENERATE THE WORD CLOUD
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#6 BUILD A BIGRAM
#You can create a data frame of bigrams (Split into word pairs)
bigrams <- docs %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2)

head(bigrams)

bigrams %>%
  count(bigram, sort=TRUE)

#A lot of uninteresting words

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#new bigram counts
bigram_counts<-bigrams_filtered %>%
  count(word1, word2, sort=TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")

bigrams_united

