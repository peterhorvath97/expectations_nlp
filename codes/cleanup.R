fomc_tokens <- read_rds('data/fomc_tokens.rds')

fomc_tokens <- fomc_tokens %>% 
  mutate(bigram = map(bigram,
                      ~.x %>% 
                        filter(str_detect(ngram, ' '))),
         trigram = map(trigram,
                       ~.x %>% 
                         filter(str_detect(ngram, ' '))),
         x = map(bigram, 
                 ~.x %>% 
                   ungroup %>% 
                   select(ngram) %>%
                   distinct() %>% 
                   pull(ngram)),
         trigram = map2(trigram, 
                        x, 
                        ~.x %>% 
                          filter(!(ngram %in% .y)))) %>% 
  select(-x)

gc()

fomc_tokens <- fomc_tokens %>% 
  mutate(tokens = map2(unigram, bigram, ~bind_rows(.x, .y)),
         tokens = map2(tokens, trigram, ~bind_rows(.x, .y))) %>% 
  select(-unigram, -bigram, -trigram)

gc()

fomc_tokens <- fomc_tokens %>% 
  mutate(tokens = map(tokens, 
                      ~.x %>% 
                        mutate(doc_id = str_remove_all(doc_id, 'doc'),
                               doc_id = as.factor(doc_id),
                               doc_id = as.numeric(doc_id))
  )) 

gc()

fomc_tokens <- fomc_tokens %>% 
  unnest(tokens) %>% 
  mutate(ngram = str_replace_all(ngram, '\\bnetari\\b', 'monetari'),
         ngram = str_replace_all(ngram, '\\bfed\\b', 'feder'),
         ngram = str_replace_all(ngram, '\\bnei\\b', 'monei')) %>% 
  group_by_at(vars(-doc_id,-ngram,-n)) %>% 
  nest() %>% 
  ungroup() %>% 
  rename(tokens = data) %>%  
  mutate(tokens = map(tokens, ~ .x %>% 
                        group_by(doc_id, ngram) %>% 
                        summarize(n = sum(n)) %>% 
                        ungroup()))


gc()


fomc_tokens <- fomc_tokens %>% 
  unnest(tokens)

sorter <- fomc_tokens %>% 
  distinct(ngram) %>% 
  mutate(sorted = str_split(ngram, ' '),
         sorted = map(sorted, ~.x %>% sort()),
         sorted = map(sorted, ~.x %>% paste(collapse = ' ')) %>% unlist())

fomc_tokens <- fomc_tokens %>% 
  inner_join(sorter) %>% 
  mutate(ngram = sorted) %>% 
  select(-sorted) %>% 
  group_by_at(vars(-doc_id,-ngram,-n)) %>% 
  nest() %>% 
  ungroup() %>% 
  rename(tokens = data) %>%  
  mutate(tokens = map(tokens, ~ .x %>% 
                        group_by(doc_id, ngram) %>% 
                        summarize(n = sum(n)) %>% 
                        ungroup()))


saveRDS(fomc_tokens, 'data/fomc_tokens.rds')
rm(fomc_tokens, sorter)
gc()