fomc_tokens <- read_rds('data/fomc_tokens_filtered.rds')
lda <- read_rds('data/lda.rds')
labels <- read_rds('data/labels.rds')
gc()




fomc_tokens <- fomc_tokens %>% 
  select(-tagged) %>% 
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
rm(lda)

gc()

pred <- pred$topics %>% 
  as_tibble()
names(pred) <- paste('topic', names(pred), sep = '_')


fomc_tokens <- fomc_tokens %>% 
  group_by_at(vars(-ngram, -n)) %>% 
  reframe(ngram = paste0(ngram, collapse = ' ')) %>% 
  select(-ftype)

uncertainty <- read_rds('data/fomc_tokens_filtered.rds') %>% 
  select(-tokens) %>% 
  rename(tokens = tagged) %>% 
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
  select(-doc_corr) %>% 
  group_by_at(vars(-tag, -n,-ngram)) %>% 
  summarize(tag = max(tag)) 

gc()

idx_df <- bind_cols(fomc_tokens, pred) %>% 
  group_by(mtg_date, pub_date, name, url) %>% 
  summarize(across(starts_with('topic'), ~mean(.x))) %>% 
  gather(key = topic, 
         value = value,
         starts_with('topic')) %>% 
  mutate(topic = str_remove_all(topic, 'topic_'),
         value = 100*value) %>% 
  rename(dist = value) %>% 
  ungroup() %>% 
  mutate(topic = as.numeric(topic)) %>% 
  full_join(labels) %>% 
  mutate(topic = theme) %>% 
  select(-theme) 


uncertainty <- bind_cols(uncertainty, pred) %>% 
  filter(tag == 1) %>% 
  group_by(mtg_date, pub_date, name, url) %>% 
  summarize(across(starts_with('topic'), ~mean(.x))) %>% 
  gather(key = topic, 
         value = value,
         starts_with('topic')) %>% 
  mutate(topic = str_remove_all(topic, 'topic_'),
         value = 100*value) %>% 
  rename(dist = value) %>% 
  ungroup() %>% 
  mutate(topic = as.numeric(topic)) %>% 
  full_join(labels) %>% 
  mutate(topic = theme) %>% 
  select(-theme) 



idx_df %>% 
  filter(name == 'ColorBook') %>% 
  select(mtg_date, url) %>% 
  distinct(url, .keep_all = T) %>% 
  filter(!str_detect(tolower(url), rebus::or('red', 'blue', 'beige', 'tealbookb', 'sup'))) %>% 
  mutate(url2 = url %>% 
           tolower() %>% 
           str_remove_all('https://www.federalreserve.gov/') %>% 
           str_remove_all('monetarypolicy') %>% 
           str_remove_all('files') %>% 
           str_remove_all('fomc') %>% 
           str_remove_all(DGT) %>% 
           str_remove_all('.pdf') %>% 
           str_remove_all('/')) %>%
  janitor::tabyl(url2)
  filter(str_detect(url2, 'gbspecial'))

idx_df <- idx_df %>% 
  mutate(url2 = tolower(url),
         url2 = ifelse(str_detect(url2, rebus::or('greenbook', 'tealbooka', 'gbpt', 'gbspecial')), 'Greenbook', NA )) %>% 
  mutate(name = ifelse(is.na(url2), name, url2),
         name = ifelse(name == 'ColorBook', 'OtherReports', name)) %>% 
  select(date = mtg_date, name, topic, dist) 
  
uncertainty <- uncertainty %>% 
  mutate(url2 = tolower(url),
         url2 = ifelse(str_detect(url2, rebus::or('greenbook', 'tealbooka', 'gbpt', 'gbspecial')), 'Greenbook', NA )) %>% 
  mutate(name = ifelse(is.na(url2), name, url2),
         name = ifelse(name == 'ColorBook', 'OtherReports', name)) %>% 
  select(date = mtg_date, name, topic, dist) 


dates <- idx_df %>% 
  filter(name == 'Transcript') %>% 
  summarize(min = min(date),
            max = max(date))

#idx_df <- read_rds('data/topic_dist_sent.rds')


topicdist <- idx_df  %>%
  group_by(date, name, topic) %>% 
  summarize(dist = mean(dist)) %>% 
  filter(name == 'Greenbook' |name == 'Transcript') %>% 
  filter(date >= dates$min,
         date <= dates$max) %>% 
  ggplot(aes(x = date, y = dist)) +
  geom_line(aes(linetype = name, color = name)) +
  facet_wrap(~topic, scales = 'free') +
  theme_minimal()+
  labs(x = '',
       y = '') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
topicdist

topic_uncertainty <- uncertainty  %>%
  group_by(date, name, topic) %>% 
  summarize(dist = mean(dist)) %>% 
  filter(name == 'Greenbook' |name == 'Transcript') %>% 
  filter(date >= dates$min,
         date <= dates$max) %>% 
  ggplot(aes(x = date, y = dist)) +
  geom_line(aes(linetype = name, color = name)) +
  facet_wrap(~topic, scales = 'free') +
  theme_minimal()+
  labs(x = '',
       y = '') +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
topic_uncertainty


saveRDS(idx_df, 'data/idx_df.rds')
saveRDS(uncertainty, 'data/uncertainty.rds')
saveRDS(topicdist, 'graphs/topicdist.rds')
saveRDS(topic_uncertainty, 'graphs/topic_uncertainty.rds')
rm(dtm, fomc_tokens, idx_df, labels, pred, dates, topicdist, uncertainty, topic_uncertainty)
gc()




