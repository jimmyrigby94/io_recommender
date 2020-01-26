library(tidyverse)
library(tidytext)
library(psych)

tokenized_data<-read_feather("Data/tokenized data.feather")

source_names<-unique(tokenized_data$`Source title`)

# Plotting Associations between Select Journals ---------------------------

tokenized_data%>%
  filter(`Source title` == "Journal of Applied Psychology"|`Source title` == "Personnel Psychology")%>%
  select(`Source title`, word, p_w_given_s)%>%
  distinct()%>%
  spread(key = `Source title`, value = p_w_given_s)%>%
  mutate_if(is.double, ~if_else(is.na(.), 0, .))%>%
  ggplot(aes(x = `Journal of Applied Psychology`, y = `Personnel Psychology`))+
  geom_point()+
  geom_abline(color = "gray40", lty = 2)+
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format())




tokenized_data%>%
  filter(`Source title` == "Organizational Research Methods"|`Source title` == "Journal of Management")%>%
  select(`Source title`, word, p_w_given_s)%>%
  distinct()%>%
  spread(key = `Source title`, value = p_w_given_s)%>%
  mutate_if(is.double, ~if_else(is.na(.), 0, .))%>%
  ggplot(aes(x = `Organizational Research Methods`, y = `Journal of Management`))+
  geom_point()+
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format())

cor_test_data<-tokenized_data%>%
      select(`Source title`, word, n_w_given_s)%>%
      distinct()

cor_test_list<-list()

for (i in source_names){
  for (j in source_names){
    
    if(i!=j){
      iter_data<-cor_test_data%>%
        filter(`Source title` == i| `Source title` == j)%>%
        spread(`Source title`, n_w_given_s)%>%
        mutate_if(is.numeric, ~if_else(is.na(.), 0, as.double(.)))
      
      cor_test_list[[paste(i, j, sep = "_")]]<-cor.test(pull(iter_data, i), pull(iter_data, j))
      
    }
  }
  
}


cor_mat<-matrix(nrow = length(source_names), ncol = (length(source_names)))

counter<-1

for(i in 1:length(source_names)){
  for(j in 1:length(source_names)){
    if(i == j){
      cor_mat[i,j]<-1
    }else{
      cor_mat[i,j]<-cor_test_list[[counter]]$estimate
      counter<-counter+1
    }
  }
}

colnames(cor_mat)<-source_names
rownames(cor_mat)<-source_names

sort(cor_mat[,3], decreasing = TRUE)

cor_mat_df<-as.data.frame(cor_mat)%>%
  rownames_to_column(var = "Journal Name A")%>%
  gather(key = "Journal Name B", value = "Word Frequency Correlation", -`Journal Name A`)


# Plot function takes one argument: Journal title
cor_mat_df%>%
  filter(`Journal Name A` == "Journal of Applied Psychology", `Journal Name B` != "Journal of Applied Psychology")%>%
  arrange(desc(`Word Frequency Correlation`))%>%
  mutate(`Journal Name B` = factor(`Journal Name B`, levels = unique(`Journal Name B`)))%>%
ggplot(aes(x = `Journal Name B`, y = `Word Frequency Correlation`))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        panel.grid = element_blank())
  
efa<-map(1:20, ~psych::fa(cor_mat, nfactors = ., n.obs = 13000, rotate = "varimax", covar = FALSE))

plot(efa[[1]]$e.values[2:77])
plot(map_dbl(efa, "EBIC"))
plot(map_dbl(efa, "ESABIC")) 
plot(map_dbl(efa, ~.$RMSEA[1]))
plot(map_dbl(efa, "TLI"))

unclass(efa[[7]]$loadings)
unclass(efa[[8]]$loadings)
unclass(efa[[9]]$loadings)


write_rds(cor_mat, "word_frequency_cor_mat.rds")
write_rds(cor_mat_df, "word_frequency_cor_mat_df.rds")

