library(gutenbergr)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(sqldf)

gatsby<-gutenberg_download(c(64317))
gatsby$text<ifelse(gatsby$text=="I", "CHAPTER 1", )

tidy_gatsby <- gatsby %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

r<-as.character(as.roman(1:9))

gatsby$t<-ifelse(str_detect(gatsby$text, r), 1, 0)
sqldf("select case when text like I then 'Chapter 1' end from gatsby where text in")

#word count
tidy_gatsby %>%
  count(word, sort = TRUE)

get_sentiments("bing")

bing_positive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

tidy_gatsby %>%
  inner_join(bing_positive) %>%
  count(word, sort = TRUE)

#needs to change for chapter filtering

#as.roman(1:9)


tidy_g <- gatsby %>%
  group_by(gutenberg_id) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#need to parse for roman numerals

tidy_g2 <- gatsby %>%
  group_by(gutenberg_id) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                as.character(as.roman(1:9))))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


gatsby_sentiment1 <- tidy_g %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index = linenumber %/% 50, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# gatsby_sentiment2 <- tidy_g %>%
#   inner_join(get_sentiments("nrc")) %>%
#   count(word, index = linenumber %/% 80, sentiment) %>%
#   pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
#   mutate(sentiment = positive - negative)


ggplot(gatsby_sentiment1, aes(index, sentiment)) +
  geom_col(fill="#7CFC00")+ggtitle("Great Gatsby Sentiment") 

# ggplot(gatsby_sentiment2, aes(index, sentiment)) +
#   geom_col(show.legend = FALSE) 


bing_word_counts <- tidy_g %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

tidy_g %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)



#Part 2

library(sentimentr)
sentiment(gatsby)
#https://www.r-bloggers.com/2020/04/sentiment-analysis-in-r-with-sentimentr-that-handles-negation-valence-shifters/


library(tnum)
tnum.authorize("mssp1.bu.edu")
tnum.getDBPathList(taxonomy="subject", levels=1)

tnum.setSpace("test2")

source("Book2TN-v6A-1.R")

tnBooksFromLines(gatsby$text, "gatsby/hw_1")
?tnBooksFromLines

tnum.getDBPathList(taxonomy="subject", levels=2)
q<-tnum.query(query = "gatsby/hw_1# has *", max=23112)
df<-tnum.objectsToDf(q)
df1<-df%>%filter(property=="text")

library(sentimentr)

sentiment=sentiment_by(df1$string.value)

ggplot(data=sentiment, aes(x=element_id, y=ave_sentiment))+geom_point()


