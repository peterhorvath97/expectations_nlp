terms(lda, 20)  

x <- terms(lda, 10) %>% 
  as_tibble() %>% 
  mutate(term = row_number()) %>% 
  gather(key = topic, value = terms, -term) %>% 
  spread(term, terms) %>% 
  mutate(topic = str_remove_all(topic, 'Topic ') %>% 
           as.numeric()) %>% 
  arrange(topic) %>% 
  print(n = 100)

source('codes/seed_data.R')

labels <- tibble(topic = 1, theme = 'Topic 1')

if(nrow(labels) < lda@k){
  labels <- labels %>% 
    bind_rows(tibble(topic = seq(nrow(labels)+1, lda@k), 
                     theme = paste('Topic', seq(nrow(labels)+1, lda@k), sep = ' '))
    )
}

top10 <- tidy(lda, matrix = 'beta') %>%
  inner_join(labels) %>% 
  mutate(topic = theme) %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
top10
saveRDS(top10, 'graphs/top10.rds')


png("graphs/wordcloud_grid.png", width = 800, height = 800)
par(mfrow = c(5, 8), mar = c(2, 1, 1, 1))
for(i in 1:lda@k){
  wordcloud::wordcloud(words = tidy(lda, matrix = 'beta') %>% 
                         filter(topic == i) %>% 
                         pull(term),
                       freq = tidy(lda, matrix = 'beta') %>% 
                         filter(topic==i) %>% 
                         pull(beta),
                       max.words = 50,
                       random.order = FALSE,
                       rot.per = 0,
                       main = 'valami')
  mtext(side = 1, line = 1, bquote(bold(.(paste('Topic', i, '-', labels[i,2], sep = ' ')))), cex = 0.8)
}
dev.off()
par(mfrow = c(1, 1))
