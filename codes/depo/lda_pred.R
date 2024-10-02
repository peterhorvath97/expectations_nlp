fomc_tokens <- read_rds('data/fomc_tokens_filtered.rds')
lda <- read_rds('data/lda.rds')
gc()



fomc_tokens <- fomc_tokens %>% 
  mutate(tokens = map(tokens,
                      ~.x %>% 
                        arrange(doc_id) %>% 
                        mutate(doc_id = as.factor(doc_id),
                               doc_id = as.numeric(doc_id))),
         doc_corr = map(tokens, ~max(.x$doc_id)) %>% unlist()) %>%  
  filter(doc_corr != -Inf) %>% 
  mutate(doc_corr = doc_corr %>% 
           accumulate(sum, .dir = 'forward') %>% 
           lag %>% 
           replace_na(0)) %>% 
  unnest(tokens) %>% 
  mutate(doc_id = doc_id + doc_corr) %>% 
  select(-doc_corr)




gc()

dtm <- fomc_tokens %>% 
  select(doc_id, ngram, n) %>% 
  cast_dtm(document = doc_id, term = ngram, value = n)

gc()

pred <- posterior(lda, dtm)

gc()

pred <- pred$topics %>% 
  as_tibble()
names(pred) <- paste('topic', names(pred), sep = '_')



fomc_tokens <- fomc_tokens %>% 
  group_by_at(vars(-ngram, -n)) %>% 
  reframe(ngram = paste0(ngram, collapse = ' ')) %>% 
  select(-ftype)

gc()

idx_df <- left_join(
bind_cols(fomc_tokens, pred) %>% 
  group_by(mtg_date, pub_date, name, url) %>% 
  summarize(across(starts_with('topic'), ~mean(.x))) %>% 
  gather(key = topic, 
         value = value,
         starts_with('topic')) %>% 
  mutate(topic = str_remove_all(topic, 'topic_'),
         value = 100*value) %>% 
  rename(dist = value) %>% 
  ungroup(),
sentiment) %>% 
  mutate(sentiment = replace_na(sentiment, 0)) %>% 
  mutate(sentiment = sentiment*dist) %>% 
  select(-url) %>% 
  rename(sent = sentiment) %>% 
  mutate(topic = as.numeric(topic)) %>% 
  full_join(labels) %>% 
  mutate(topic = theme) %>% 
  select(-theme)


dates <- idx_df %>% 
  filter(name == 'Transcript') %>% 
  summarize(min = min(mtg_date),
            max = max(mtg_date))

idx_df <- read_rds('data/topic_dist_sent.rds')


topicdist <- idx_df  %>%
  group_by(mtg_date, name, topic) %>% 
  summarize(dist = mean(dist), sent = mean(sent)) %>% 
  filter(name == 'ColorBook' |name == 'Transcript') %>% 
  mutate(name = ifelse(name == 'ColorBook', 'Staff Report', name)) %>% 
  filter(mtg_date >= dates$min,
         mtg_date <= dates$max) %>% 
  ggplot(aes(x = mtg_date, y = dist)) +
  geom_line(aes(linetype = name, color = name)) +
  facet_wrap(~topic, scales = 'free') +
  theme_minimal()+
  labs(x = '',
       y = '') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))



saveRDS(idx_df, 'data/topic_dist_sent.rds')
saveRDS(topicdist, 'graphs/topicdist.rds')
rm(dtm, fomc_tokens, idx_df, labels, lda, pred, sentiment, dates, topicdist, lexicon, i)





