###########################      PROJECT 2  -  TEXT MINING    ################################
library(textclean)
library(twitteR)
library(tm)
library(stopwords)
library(wordcloud)
library(topicmodels)
library(ggplot2)
library(data.table)
library(sentimentr)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(ggcorrplot)
library(Hmisc)
library(VIM)
library(stringr)

df <- read.csv(file.choose(), na.strings=c("", "NA"))

dim(df)
summary(df)
glimpse(df)


##################  CLEANING  #################

#Not certain we need the columns: airline_senitment_confidence, negativereason_confidence, airline_sentiment-gold, negativereason_gold, tweet_created or user_timezone
#Removing these

df <- subset(df, select = -c(airline_sentiment_confidence, negativereason_confidence, airline_sentiment_gold, negativereason_gold, tweet_created, user_timezone))
glimpse(df)
summary (df)

#Looking for NA's
 
sum(is.na(df))  #23816

missing_values <- aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=0.5, gap=2, ylab=c("Histogram of missing data","Pattern"))
# All Na's are located in the tweet_coord, negativereason and tweet_location columns


#Exploring why the negative comment reason have NA
NA_Reasons <- df %>%
  select(airline_sentiment, negativereason, airline) %>%
  filter(is.na(negativereason))
summary(NA_Reasons)
# NA given to positive and neutral reviews

#Replacing NA with blank spaces
na_vals1 <- which(is.na(df$negativereason)) #Rows where NAs are located
df$negativereason <- as.character(df$negativereason) #first convert to character strings
df[na_vals1,]$negativereason <- ""
df$negativereason <- as.factor(df$negativereason)  #convert back to factors

summary(df$negativereason)
#Clean!

#Exploring NA Values in tweet_coord and tweet_location
na_locals <- df %>%
  select(tweet_coord, tweet_location)
summary(na_locals)
#Lots of NA in coordinates, will remove column and treat tweet location
df <- subset(df, select = -c(tweet_coord))  #removing coordinate column

na_vals2 <- which(is.na(df$tweet_location)) #Rows where NAs are located
df$tweet_location <- as.character(df$tweet_location) #first convert to character strings
df[na_vals2,]$tweet_location <- "Unspecified"
df$tweet_location <- as.factor(df$tweet_location)  #convert back to factors

summary(df$tweet_location)

#################################################
#### What should we do to organize these???  ####
tweet_locations <- df %>%
  select(tweet_location) %>%
  arrange(tweet_location)
#################################################



#############  Data Exploration  ################

###  Looking at the count of airline sentiments per airline  ###
sentiments <- df %>%
  select(airline, airline_sentiment) %>%
  group_by(airline, airline_sentiment) %>%
  summarise(freq = n())

ggplot(sentiments, aes(x = airline, y = freq, fill = airline_sentiment)) + 
  geom_bar(stat="identity", position = position_dodge()) + 
  theme_set(theme_bw()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Distribution of Airline Sentiments", x = 'Airline', y = 'Frequency')
  


###  Looking at the distribution of negative comments  ###
neg_reasons <- df %>%
  filter(airline_sentiment == "negative") %>%
  select(airline, negativereason) %>%
  group_by(airline, negativereason) %>%
  summarise(freq = n())

ggplot(neg_reasons, aes(x = negativereason, y = freq)) + 
  geom_bar(stat="identity", fill="blue") + 
  theme_set(theme_bw()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Reasons for the Negative Sentiments", x = 'Negative Reasons', y = 'Frequency') +
  facet_wrap(~ airline)


###  Looking at retweet counts and airline
retweet_1 <- df %>%
  select(airline, airline_sentiment, retweet_count) %>%
  group_by(airline, airline_sentiment) %>% 
  summarise(total = sum(retweet_count))

ggplot(retweet_1, aes(x = airline, y = total, fill = airline_sentiment)) + 
  geom_bar(stat="identity", position = position_dodge()) + 
  theme_set(theme_bw()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Total number of Retweets by Airline", x = 'Airline', y = 'Number of Retweets')

###  Looking at retweet counts and negative reasons
retweet_2 <- df %>%
  filter(airline_sentiment == "negative") %>%
  select(airline, negativereason, retweet_count) %>%
  group_by(airline, negativereason) %>%
  summarise(total = sum(retweet_count))

ggplot(retweet_2, aes(x = negativereason, y = total)) + 
  geom_bar(stat="identity", fill="blue") + 
  theme_set(theme_bw()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Total number of Retweets by Negative Reason", x = 'Negative Reasons', y = 'Number of Retweets') +
  facet_wrap(~ airline)


#retweet count for positive reasons

retweet_3 <- df %>%
  filter(airline_sentiment == "positive") %>%
  select(airline, retweet_count) %>%
  group_by(airline) %>%
  summarise(total = sum(retweet_count))


ggplot(retweet_3, aes(x = airline, y= total)) +
  geom_bar(stat= "identity", fill = "blue") +
  theme_set(theme_bw()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Number of Retweets by Positive Sentiment", x = "Airline", y = "Number of Retweets")



##Exploring the negative sentiments in the reviews

library(tidytext)
library(textdata)

review <- tidytext::unnest_tokens(read.csv(file.choose(), stringsAsFactors = FALSE), word, text)
data(stop_words)

review <- review %>%#remove stop words
  anti_join(stop_words)

review %>%
  count(word, sort=TRUE)

###counting negative sentiments
negative <- get_sentiments('bing') %>%
  filter(sentiment == "negative")

neg_reviews <- review %>%
  inner_join(negative) %>%
  count(word, sort=TRUE)

head(neg_reviews, 10)

####Put word cloud here

###counting positive sentiments
positive <- get_sentiments('bing') %>%
  filter(sentiment == "positive")

pos_reviews <- review %>%
  inner_join(positive) %>%
  count(word, sort=TRUE)

head(pos_reviews, 10)


#unsurprisingly, there are almost half as many positive reviews as there are negative ones

#Let's look at a wordcloud of our most prominent sentiments
pal <- brewer.pal(10, "Paired")

neg_reviews <- review %>%
  inner_join(negative) %>%
  count(word) %>%
  with(wordcloud(word, n,
                 rot.per = .15,
                 scale=c(5, .3),
                 max.words = 50,
                 random.order = F,
                 random.color = F,
                 colors = pal))

pos_reviews <- review %>%
  inner_join(positive) %>%
  count(word) %>%
  with(wordcloud(word, n,
                 rot.per = .15,
                 scale = c(5, .3),
                 max.words = 50,
                 random.order = F,
                 random.color = F,
                 colors = pal))

#Let's look at which airlines had the most complaints with delayed flights
#First, we'll change all 'delay' and 'delays' to 'delayed'

review$word[grepl('delays', review$word)] <- 'delayed'
review$word[grepl('delay', review$word)] <- 'delayed'

#next, we plot the data for delayed complaints by airline

delayed <- review %>%
  filter(word == 'delayed') %>%
  select(airline) %>%
  group_by(airline) %>%
  summarise(freq = n())

ggplot(delayed, aes(x = airline, y = freq)) +
  geom_bar(stat = 'identity', fill='blue') +
  theme_set(theme_bw()) +
  theme(axis.title.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Delayed Complaints by Airline", x="Airline", y="Number of Complaints")


#we can see 'refund' is a pretty high pos review, let's check how many people were refunded by airline
sum(review$word=='refunded')#refunded shows up 19 times. Let's change that to refund

review$word[grepl('refunded', review$word)] <- 'refund'

refund <- review %>%
  filter(word == 'refund') %>%
  select(airline) %>%
  group_by(airline) %>%
  summarise(freq = n())

ggplot(refund, aes(x = airline, y = freq)) +
  geom_bar(stat = 'identity', fill='red') +
  theme_set(theme_bw()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Refunded Tickets by Airline", x="Airline", y="Number of Recorded Refunds")


#let's see if certain words have an impact on sentiment
#we'll subset the data to get rid of certain columns

review <- subset(review, select = -c(airline_sentiment_confidence, negativereason_confidence, airline_sentiment_gold, negativereason_gold, tweet_created, user_timezone))

delayed_review <- review %>%
  filter(word=='delayed') %>%
  select(airline, airline_sentiment) %>%
  group_by(airline, airline_sentiment) %>%
  summarise(freq = n())


ggplot(delayed_review, aes(x = airline, y = freq, fill = airline_sentiment)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  theme_set(theme_bw()) +
  theme(axis.title.x = element_text(angle = 0, hjust = 1)) +
  labs(title="Distribution of Airline Sentiment with Delays", x="Airline", y="Frequency")

#we'll check how airline sentiment was affected for people who were given refunds

refund_review <- review %>%
  filter(word == 'refund') %>%
  select(airline, airline_sentiment) %>%
  group_by(airline, airline_sentiment) %>%
  summarise(freq = n())

ggplot(refund_review, aes(x=airline, y=freq, fill=airline_sentiment)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  theme_set(theme_bw()) +
  theme(axis.title.x = element_text(angle = 0, hjust = 1)) +
  labs(title="Distribution of Airline Sentiment with Refunds", x="Airline", y="Frequency")




