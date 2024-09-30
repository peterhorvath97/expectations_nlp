fomc_text_pos <- read_rds('data/fomc_text_pos.rds') %>% 
  select(-postags)

gc()

lexicon <- lexicon_loughran() %>% 
  filter(sentiment %in% c('negative', 'positive')) %>% 
  distinct(word, sentiment) %>% 
  rename(ngram = word) 



sentiment <- fomc_text_pos %>%
  unnest(text) %>% 
  unnest_tokens(output = "ngram",
                input = "value",
                token = "ngrams",
                n = 1) %>% 
  group_by_all() %>% 
  count() %>% 
  anti_join(stop_words, by = c('ngram' = 'word')) %>% 
  full_join(lexicon) %>%   
  filter(!is.na(mtg_date)) %>%  
  mutate(sentiment = replace_na(sentiment, 'neutral')) %>% 
  spread(sentiment, n) %>% 
  mutate(across(c(negative, neutral, positive), ~replace_na(.x, 0))) %>% 
  group_by_at(vars(-negative,-neutral,-positive,-ngram,-doc_id)) %>% 
  summarize(negative = mean(negative),
            positive = mean(positive)) %>% 
  mutate(sentiment = 100*(negative-positive)) %>% 
  select(-negative,-positive)

rm(fomc_text_pos)
gc()




sentiment <- sentiment %>% 
  mutate(url2 = tolower(url),
         url2 = ifelse(str_detect(url2, rebus::or('greenbook', 'tealbooka', 'gbpt', 'gbspecial')), 'Greenbook', NA )) %>% 
  mutate(name = ifelse(is.na(url2), name, url2),
         name = ifelse(name == 'ColorBook', 'OtherReports', name)) %>% 
  select(date = mtg_date, name, sentiment) %>% 
  group_by(date, name) %>% 
  summarize(sentiment = mean(sentiment))



saveRDS(sentiment, 'data/sentiment.rds')

rm(fomc_text_pos, sentiment, lexicon)
gc()
