master%>%
  filter(`Source title` == "Journal of Applied Psychology"|`Source title` == "Personnel Psychology")%>%
  select(`Source title`, word, p_w_given_s)%>%
  distinct()%>%
  spread(key = `Source title`, value = p_w_given_s)%>%
  mutate_if(is.double, ~if_else(is.na(.), 0, .))%>%
  ggplot(aes(x = `Journal of Applied Psychology`, y = `Personnel Psychology`))+
  geom_point()+
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format())




master%>%
  filter(`Source title` == "Journal of Applied Psychology"|`Source title` == "Journal of Management")%>%
  select(`Source title`, word, p_w_given_s)%>%
  distinct()%>%
  spread(key = `Source title`, value = p_w_given_s)%>%
  mutate_if(is.double, ~if_else(is.na(.), 0, .))%>%
  ggplot(aes(x = `Journal of Applied Psychology`, y = `Journal of Management`))+
  geom_point()+
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format())
