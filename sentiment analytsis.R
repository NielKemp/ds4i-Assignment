load('complaints.RData')
str(complaints)

#load required packages
library(tidyverse)
library(stringr)
library(tidytext)
library(lubridate)


#check counts of each type of complaint
complaints%>%group_by(product)%>%summarize(count = n(), settles = sum(consumer_compensated))%>%mutate(ratio = settles/count)
str(complaints)

str(complaints)

#unnest data
rawTidy_ <- unnest_tokens(complaints, words,consumer_complaint_narrative, token='words')

#plot some common words (also do this per product and for compensated/noncompensated)
rawTidy_ %>% group_by(product,words) %>%
  summarize(count = n())%>%
  arrange(product,words, count) %>%
  top_n(10) %>%
  ggplot(aes(words,count)) +geom_col()+facet_wrap(~product)

#rebuild data and remove stop words
rawTidy_ <- complaints%>%
  unnest_tokens(word, consumer_complaint_narrative, token='words')%>%
  filter(!word %in% stop_words$word, str_detect(word,"[a-z]"))



#replot after stop words have been removed
rawTidy_ %>% group_by(product,word) %>%
  summarize(count = n())%>%
  arrange(product,word, count) %>%
  top_n(5) %>%
  ggplot(aes(word,count)) +geom_col()+facet_wrap(~product)

#significantly different words for each product
#credit_card and debt_collection share words
#mortgage has a near unique set.
#credit reporting shares some words with creditcards and debt collection
#bank account or service has a nearly unique set.

#add sentinments from the lexicons
sentiTidy <- rawTidy_ %>%
  left_join(get_sentiments('bing'))%>%
  mutate(sentiment =ifelse(is.na(sentiment),"neutral",sentiment))

#clean up dates
sentiFin <- sentiTidy %>%mutate(date = parse_datetime(date_received, "%m/%d/%Y")) %>%mutate(yearMonth = make_date(year(date), month(date)))
                                                                                            
            
#plot over time, need to change to relative frequency.


sentiFin%>%
  group_by(yearMonth,product,sentiment)%>%
  summarize(n=n())%>%
  mutate(total=sum(n), relFreq = n/total)%>%
  filter(sentiment!='neutral')%>%
  ggplot(aes(x=yearMonth,y=relFreq, colour=sentiment))+geom_line()+facet_wrap(~product)
