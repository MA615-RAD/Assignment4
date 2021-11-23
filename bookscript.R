library(gutenbergr)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(reshape2)
library(wordcloud)

gatsby<-gutenberg_download(c(64317))



tidy_gatsby <- gatsby %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

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
  count(word, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# gatsby_sentiment2 <- tidy_g %>%
#   inner_join(get_sentiments("nrc")) %>%
#   count(word, index = linenumber %/% 80, sentiment) %>%
#   pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
#   mutate(sentiment = positive - negative)


ggplot(gatsby_sentiment1, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) 

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
