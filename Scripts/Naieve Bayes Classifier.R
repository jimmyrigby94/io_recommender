library(feather)
library(quanteda)

master<-read_feather("Data/master.feather")

master<-master%>%
  add_count(`Source title`)%>%
  filter(n>100)

corpus_master<-corpus(master$Abstract, docvars = master%>%select(Year, `Source title`))

set.seed(1234)
n_articles<-ndoc(corpus_master)
train<-sample(1:n_articles,  size = .5*n_articles, replace = FALSE)
test<-(1:n_articles)[!1:n_articles %in% train]

docvars(corpus_master, "id")<-1:n_articles

train_corpus<-corpus_subset(corpus_master, id %in% train) %>%
      dfm(stem = TRUE)

test_corpus<-corpus_subset(corpus_master, !id %in% train) %>%
  dfm(stem = TRUE)


dfmat_matched <- dfm_match(test_corpus, features = featnames(train_corpus))


m1<-textmodel_nb(train_corpus, docvars(train_corpus, "Source title"))

m1$prior
m1$smooth
m1$distribution

actual_class <- docvars(dfmat_matched, "Source title")
predicted_class <- predict(m1, newdata = dfmat_matched)

conf_mat<-table(actual_class, 
      predicted_class)%>%
  caret::confusionMatrix()
  
