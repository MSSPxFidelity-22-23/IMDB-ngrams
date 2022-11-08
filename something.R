library(tidyverse)
library(stopwords)

imbd <- read.csv("IMDB Dataset.csv")

imbd_test <- imbd[1:100,]
#https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25
imbd_test$review <- gsub("<br />", " ", imbd_test$review)
imbd_test <- rowid_to_column(imbd_test,"id")
text_cleaning_tokens <- imbd_test %>% 
  tidytext::unnest_tokens(word, review)
text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
  anti_join(tibble(word = stopwords()))

tokens <- text_cleaning_tokens %>% filter(!(word==""))
tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""
tokens <- tidyr::unite(tokens, text,-id,-sentiment,sep =" " )
tokens$text <- trimws(tokens$text)

words_count <- text_cleaning_tokens %>%
  count(word, sort = TRUE)

