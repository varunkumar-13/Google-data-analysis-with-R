library(rtweet)
library(dplyr)
library(ggplot2)
library(twitteR)
library(tidyverse)
library(stringr)
library(tidytext)

consumer_key <-"HJFjGj4jePPbVlmHRwqUUA2hR"
consumer_secret <-"iMwL77EecWmwJcjZ2IJcACtbKeqrU330pNARSrJDTznLblfyA1"
access_token<-"1332340495473803265-3zFRSVUyjLTj6N2VMaIG3PNjA8bcbd"
access_secret <-"J80cCRB5jqQp0tdeOTzjNC0r3k9X9U9zcVVmllJT4fVhg"


twitter_token = rtweet::create_token(app ='DMDA',
                                     consumer_key ="HJFjGj4jePPbVlmHRwqUUA2hR",
                                     consumer_secret ="iMwL77EecWmwJcjZ2IJcACtbKeqrU330pNARSrJDTznLblfyA1",access_token="1332340495473803265-3zFRSVUyjLTj6N2VMaIG3PNjA8bcbd",
                                     access_secret="J80cCRB5jqQp0tdeOTzjNC0r3k9X9U9zcVVmllJT4fVhg"
)


# TESTING LOCATION FILTER

View(trends_available() %>% filter(countryCode=="IN"))


# TESTING WOEID (Where On Earth IDentifier)

trending_tweets<-get_trends(woeid =23424848)
View(trending_tweets)


top_tweet<-head(trending_tweets$trend,1)
View(top_tweet)




tweets<-search_tweets('Iran',n=100,include_rts = FALSE,`-filter` = "replies",
                        lang = "en")
View(tweets)



# Frequency of Tweets time series graph

ts_plot(tweets, "hours") +
                                                       labs(x = NULL, y = NULL,
                                                            title = "Frequency of tweets with a #Iran hashtag",
                                                            subtitle = paste0(format(min(tweets$created_at), "%d %B %Y"), " to ", format(max(tweets$created_at),"%d %B %Y")),
                                                            caption = "Data collected from Twitter's REST API via rtweet") +
                                                       theme_minimal()




#To Find Most Frequent Words used in Tweets

words <- tweets %>%
                                                       mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
                                                              text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
                                                              text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
                                                       unnest_tokens(word, text, token = "tweets") %>%
                                                       filter(!word %in% stop_words$word,
                                                              !word %in% str_remove_all(stop_words$word, "'"),
                                                              str_detect(word, "[a-z]"),
                                                              !str_detect(word, "^#"),         
                                                              !str_detect(word, "@\\S+")) %>%
                                                       count(word, sort = TRUE)

library(wordcloud) 
words %>% 
                                                       with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = "#F29545"))


tweets %>% 
                                                       unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
                                                       filter(str_detect(mentions, "^@")) %>%  
                                                       count(mentions, sort = TRUE) %>%
                                                       top_n(10)




mytext<-sub(".", "",top_tweet)
print(mytext)
                              


