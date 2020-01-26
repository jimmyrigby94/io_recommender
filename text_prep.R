library(feather)
library(tidytext)
library(tidyverse)
library(scales)

# Reading in File ---------------------------------------------------------

master<-read_feather("Data/master.feather")

custom_stop_words<-c("american", "apa", "association", "psychonomic", "society", "study", "results", "research", "rights", "reserved",
                     "psychological", "data", "findings", "studies", "based", "model", "effects", "related", "social", "performance", 
                     "found", "discussed", "analysis", "elsevier", "copyright", "wiley", "wiley", "john", "sons", "francis", "llc", 
                     "rights", "taylor", "organizational", "industrial", "periodicals", "limited", "elsevier", "management", "author", 
                     "british", "findings", "hogrefe", "llc", "taylor", "academy", "emerald","publishing", "sage", "database", "record", 
                     "article")

# Current format - nrow = number of articles ------------------------------
master<-master%>%
  select(Year, `Source title`, Title, Abstract)%>%
  # Dropping missing abstracts
  filter(Abstract != "[No abstract available]")%>%
  # Removing duplicate observations 
  distinct(`Source title`, Title, Year, .keep_all = TRUE)%>%
  # Counting the number of articles from each source
  add_count(`Source title`, name = "n_source")%>%
  # Dropping out low frequency sources
  filter(n_source>=100)%>%
  # Counting the total number of articles
  mutate(n_articles = n())%>%
  # Dropping stop words from master data: nrow() = \sum_i^n n_words_i n = number of articles--------
  unnest_tokens(word, Abstract)%>%
  # Dropping standard and and custom stop words
  filter(! word %in% stop_words$word)%>%
  filter(! word %in% custom_stop_words)%>%
  # Retaining all numbers and symbols
  filter(str_detect(word, "^[[a-z]]+"))%>%
  # Preventing critereon leakage by removing all words that are directly associated with the source title. 
  filter(! word %in% tolower(str_split(`Source title`, " ")))%>%
  # Counting the number of times a word was used in each article; nrow = \sum_i^n unique words_i n = number of articles ---
  count(n_articles, Year, `Source title`, n_source, Title, word)%>%
  # Counting the number of times a word is used within a source title
  group_by(`Source title`, word)%>%
  mutate(n_w_given_s = n(),
         p_w_given_s = n_w_given_s/n_source)%>%
  ungroup()%>%
  # Counting the number of times the words are used overall
  group_by(word)%>%
  mutate(n_word = n(),
         p_word = n_word/n_articles,
         p_source = n_source/n_articles)%>%
  ungroup()

# Used to identify additional stopwords related to publishers/copyrights/non-informative things
# master%>%
#   select(word, p_w_given_s)%>%
#   distinct()%>%
#   arrange(desc(p_w_given_s))%>%
#   View()


write_feather(master, "Data/tokenized data.feather")



