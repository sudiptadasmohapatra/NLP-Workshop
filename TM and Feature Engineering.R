#******Natural Language Processing using R******#


#2. TEXT MINING AND FEATURE ENGINEERING

#Load Libraries
path <- "~/Desktop/NLP/"
setwd(path)
library(data.table)
library(jsonlite)
library(rjson)
library(purrr)
library(RecordLinkage)
library(stringr)
library(tm)
library(NLP)

#load data
traind <- fromJSON(file="train.json")
test <- fromJSON(file="test.json")

#convert json to data table (using map_at function from purrr package)
vars <-setdiff(names(traind), c("photos", "features"))
train <- map_at(traind, vars, unlist) %>% as.data.table()
test <- map_at(test, vars, unlist) %>% as.data.table()

#since we are interested only in test variables, extract only text features
train <- train[,.(listing_id,features, description,street_address,display_address,interest_level)]
test <- test[,.(listing_id,features,street_address,display_address,description)]

#understanding data
dim(train)
dim(test)
head(train)
head(test)
sapply(train,class)
sapply(test,class)

#FEATURE ENGINEERING
#Join Train and Test data
test[,interest_level := "None"]
tdata <- rbindlist(list(train,test))

#fill empty values in the list
tdata[,features := ifelse(map(features, is_empty),"aempty",features)]

#count number of features per listing
tdata[,feature_count := unlist(lapply(features, length))]

#count number of words in description
tdata[,desc_word_count := str_count(description,pattern = "\\w+")]

#count total length of description
tdata[,desc_len := str_count(description)]

#similarity between address
tdata[,lev_sim := levenshteinDist(street_address,display_address)]

dim(tdata)

#Transform "Features" into one feature per row format 
#extract variables from features
fdata <- data.table(listing_id = rep(unlist(tdata$listing_id), lapply(tdata$features, length)), features = unlist(tdata$features))
head(fdata)

#convert features to lower
fdata[,features := unlist(lapply(features, tolower))]

#Calculate the count of features and remove features which are occurring less than 100 times in the data
#calculate count for every feature
fdata[,count := .N, features]
fdata[order(count)][1:20]

#keep features which occur 100 or more times
fdata <- fdata[count >= 100]

#Convert each feature into a separate column to use those as variables in model training. 
#To accomplish that, use the dcast function.

#convert columns into table`<br/>
fdata <- dcast(data = fdata, formula = listing_id ~ features, fun.aggregate = length, value.var = "features")
dim(fdata)

#Extract features from Description Variable (use tm package)
#create a corpus of descriptions
text_corpus <- Corpus(VectorSource(tdata$description))
#check first 4 documents
inspect(text_corpus[1:4])

#the corpus is a list object in R of type CORPUS
print(lapply(text_corpus[1:2], as.character))

#CLEAN CORPUS
#Clean the data of "br" - it it just noise
dropword <- "br"

#remove br
text_corpus <- tm_map(text_corpus,removeWords,dropword)
print(as.character(text_corpus[[1]]))

#change to lower case
text_corpus <- tm_map(text_corpus, tolower)
print(as.character(text_corpus[[1]]))

#remove punctuation
text_corpus <- tm_map(text_corpus, removePunctuation)
print(as.character(text_corpus[[1]]))

#remove numbers
text_corpus <- tm_map(text_corpus, removeNumbers)
print(as.character(text_corpus[[1]]))

#remove whitespaces
text_corpus <- tm_map(text_corpus, stripWhitespace)
print(as.character(text_corpus[[1]]))

#remove stopwords
text_corpus <- tm_map(text_corpus, removeWords, c(stopwords('english')))
print(as.character(text_corpus[[1]]))

#convert to text document
text_corpus <- tm_map(text_corpus, PlainTextDocument)

#perform stemming - this should always be performed after text doc conversion
text_corpus <- tm_map(text_corpus, stemDocument,language = "english")
print(as.character(text_corpus[[1]]))
text_corpus[[1]]$content

install.packages("SnowballC")
library(SnowballC)

#convert to document term matrix
docterm_corpus <- DocumentTermMatrix(text_corpus)
dim(docterm_corpus)

#remove terms that are 95% sparse or more
new_docterm_corpus <- removeSparseTerms(docterm_corpus,sparse = 0.95)
dim(new_docterm_corpus)

#Find frequent terms
colS <- colSums(as.matrix(new_docterm_corpus))
length(colS)
doc_features <- data.table(name = attributes(colS)$names, count = colS)

#most frequent and least frequent words
doc_features[order(-count)][1:10] #top 10 most frequent words
doc_features[order(count)][1:10] #least 10 frequent words


#Visualize the terms with simple plots
library(ggplot2)
library(ggthemes)

ggplot(doc_features[count>20000],aes(name, count)) + 
  geom_bar(stat = "identity",fill='lightblue',color='black')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_economist()+
  scale_color_economist()

#check association of terms of top features
findAssocs(new_docterm_corpus,"street",corlimit = 0.5)
findAssocs(new_docterm_corpus,"new",corlimit = 0.5)

#create wordcloud
library(wordcloud)
wordcloud(names(colS), colS, min.freq = 100, scale = c(6,.1), colors = brewer.pal(6, 'Dark2'))

wordcloud(names(colS), colS, min.freq = 5000, scale = c(6,.1), colors = brewer.pal(6, 'Dark2'))

#MODELING
#Convert matrix into data frame and merge with new features from "feature" column

#create data set for training
processed_data <- as.data.table(as.matrix(new_docterm_corpus))

#data1
#combining the data using listing_id
data_one <- cbind(data.table(listing_id = tdata$listing_id, interest_level = tdata$interest_level),processed_data)

#merging the features
data_one <- fdata[data_one, on="listing_id"]

#split the data set into train and test
train_one <- data_one[interest_level != "None"]
test_one <- data_one[interest_level == "None"]
test_one[,interest_level := NULL]
rm(data_one,processed_data)

#modeling data 1
train_one[,interest_level := as.factor(interest_level)]
train_one[,interest_level := as.integer(as.factor(interest_level))-1]
#high = 0, low = 1, medium = 2

library(caTools)
library(xgboost)

#returns 60% indexes from train data
sp <- sample.split(Y = train_one$interest_level,SplitRatio = 0.6)

#create data for xgboost
xg_val <- train_one[sp]
listing_id <- train_one$listing_id
target <- train_one$interest_level
xg_val_target <- target[sp]

d_train <- xgb.DMatrix(data = as.matrix(train_one[,-c("listing_id","interest_level"),with=F]),label = target)
d_val <- xgb.DMatrix(data = as.matrix(xg_val[,-c("listing_id","interest_level"),with=F]), label = xg_val_target)
d_test <- xgb.DMatrix(data = as.matrix(test_one[,-c("listing_id"),with=F]))

param <- list(booster="gbtree",
              objective="multi:softprob",
              eval_metric="mlogloss",
              #nthread=13,
              num_class=3,
              eta = .02,
              gamma = 1,
              max_depth = 4,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .5
)


watch <- list(val=d_val, train=d_train)
xgb2 <- xgb.train(data = d_train,
                  params = param,
                  watchlist=watch,
                  # nrounds = xgb2cv$best_ntreelimit
                  nrounds = 500,
                  print_every_n = 10
) #validation - 0.692980

#Create Predictions on the test data and save this as xgb_textmining.csv file
xg_pred <- as.data.table(t(matrix(predict(xgb2, d_test), nrow=3, ncol=nrow(d_test))))
colnames(xg_pred) <- c("high","low","medium")
xg_pred <- cbind(data.table(listing_id = test$listing_id),xg_pred)
fwrite(xg_pred, "xgb_textmining.csv") 

