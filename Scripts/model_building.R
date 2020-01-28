library(quanteda)
library(glmnet)
library(randomForest)
library(doParallel)
library(mlr3)
library(tidyverse)
library(tidytext)
library(feather)
library(data.table)
library(mlr3learners)

tokenized_data<-read_feather("Data/tokenized data.feather")

token

tokenized_data$article_id<-tokenized_data%>%
  group_by(Title)%>%
  group_indices()

tokenized_data<- tokenized_data%>%
  select(`Source title`, article_id, Year, word, n)%>%
  cast_dtm(document = article_id, term = word, value = n)

tokenized_data<-tokenized_data%>%
  as.matrix()


task <- mlr3::TaskClassif$new(id = )
learner <- mlr3::mlr_learners$get("classif.kknn")

learner$param_set$values = list(scale = TRUE)

# Casting to sparse matrix -----------------------------------------------------

dtf<- master%>%
  cast_sparse(source_title_article, word, n)


# Creating outcome vector -------------------------------------------------

y_vec<-master%>%
  ungroup()%>%
  select(source_title_article, `Source title`)%>%
  distinct()

outcome<-y_vec%>%
  mutate(id = 1:n(),
         `Source title` = factor(`Source title`),
         value = 1)%>%
  select(id, `Source title`, value)%>%
  spread(key = `Source title`, value= value)%>%
  mutate_all(~if_else(is.na(.), 0, as.double(.)))%>%
  select(-id)%>%
  as.matrix()

registerDoParallel(12)
m1<-cv.glmnet(y = outcome[,2:ncol(outcome)], x = dtf, family = "multinomial", nfolds = 10, parallel=TRUE, lambda = seq(-7, -5, by = .1))

removeSparse
plot(m1)

m2<-randomForest(x = as.matrix(dtf), y = as.factor(pull(y_vec, `Source title`)))
