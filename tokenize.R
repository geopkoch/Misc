#tokenize a dataframe

tokenize <- function(df, column, sentiment.type = "afinn", sentimentremoved = "stopwords.sentimentremoved"){
  if (!require("pacman")) install.packages("pacman")
  p_load(dplyr,magrittr,tidytext,SnowballC)
  if(exists(sentimentremoved)) 
    sentimentremoved
  else stopwords.sentimentremoved<- anti_join(stop_words,get_sentiments(sentiment.type)) #give me all stop words that are not related to sentiment
  df <- df %>%
    unnest_tokens_("word",column) %>%
    mutate(Question = column) %>%
    anti_join(stopwords.sentimentremoved, by = "word") %>% #give me all words that are not stop words
    filter(!str_detect(word,"[0-9]")) %>%
    filter(!str_detect(word,"[[:punct:]]")) %>%
    mutate(word_stem = wordStem(word))
}
