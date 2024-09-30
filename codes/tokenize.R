fomc_text_pos <- read_rds('data/fomc_text_pos.rds')


fomc_text_pos <- fomc_text_pos %>% 
  mutate(unigram = map(text,
                       ~unnest_tokens(.x,
                                      output = "ngram",
                                      input = "value",
                                      token = "ngrams",
                                      n = 1)),
         bigram = map(text,
                      ~unnest_tokens(.x,
                                     output = "ngram",
                                     input = "value",
                                     token = "ngrams",
                                     n = 2)),
         trigram = map(text,
                       ~unnest_tokens(.x,
                                      output = "ngram",
                                      input = "value",
                                      token = "ngrams",
                                      n = 3))) %>% 
  select(-text)


fomc_text_pos <- fomc_text_pos %>% 
  mutate(unigram = map2(unigram, postags, ~inner_join(.x,
                                                      .y,
                                                      by = c('doc_id', 'ngram' = 'token')) %>% 
                          filter(upos  %in%  c('NOUN')) %>% 
                          select(-upos) %>% 
                          mutate(ngram = removeWords(ngram, stopwords('en')) %>% 
                                   stripWhitespace() %>% 
                                   str_remove_all('^ ') %>% 
                                   str_remove_all(' $')) %>% 
                          filter(ngram != '') %>% 
                          mutate(ngram = stem_strings(ngram)) %>% 
                          group_by(doc_id, ngram) %>% 
                          count()),
         bigram = map2(bigram, postags, ~inner_join(.x %>% 
                                                      group_by(doc_id) %>% 
                                                      mutate(token_id = row_number()) %>% 
                                                      separate_rows(ngram, sep = ' '), 
                                                    .y,
                                                    by = c('doc_id', 'ngram' = 'token')) %>% 
                         group_by(doc_id, token_id) %>% 
                         summarize(ngram = paste0(ngram, collapse = ' '),
                                   upos = paste0(upos, collapse = ' ')) %>%   
                         filter(upos  %in%  c('ADJ NOUN',
                                              'NOUN NOUN',
                                              'NUM NOUN')) %>%
                         select(-token_id, -upos) %>% 
                         mutate(ngram = removeWords(ngram, stopwords('en')) %>% 
                                  stripWhitespace() %>% 
                                  str_remove_all('^ ') %>% 
                                  str_remove_all(' $')) %>% 
                         filter(ngram != '') %>% 
                         mutate(ngram = stem_strings(ngram)) %>% 
                         group_by(doc_id, ngram) %>% 
                         count() %>% 
                         filter(!str_detect(ngram, "\\b(\\w+)\\b\\s+\\b\\1\\b"))),
         trigram = map2(trigram, postags, ~inner_join(.x %>% 
                                                        group_by(doc_id) %>% 
                                                        mutate(token_id = row_number()) %>% 
                                                        separate_rows(ngram, sep = ' '), 
                                                      .y,
                                                      by = c('doc_id', 'ngram' = 'token')) %>% 
                          group_by(doc_id, token_id) %>% 
                          summarize(ngram = paste0(ngram, collapse = ' '),
                                    upos = paste0(upos, collapse = ' '))  %>% 
                          filter(upos %in%  c('ADJ ADJ NOUN',
                                              'ADJ NOUN NOUN',
                                              'ADJ NUM NOUN', 
                                              'NOUN ADJ NOUN', 
                                              'NOUN NOUN NOUN',
                                              'NUM NOUN NOUN',
                                              'NOUN ADP NOUN')) %>% 
                          select(-token_id, -upos) %>% 
                          mutate(ngram = removeWords(ngram, stopwords('en')) %>% 
                                   stripWhitespace() %>% 
                                   str_remove_all('^ ') %>% 
                                   str_remove_all(' $')) %>% 
                          filter(ngram != '') %>% 
                          mutate(ngram = stem_strings(ngram)) %>% 
                          group_by(doc_id, ngram) %>% 
                          count() %>% 
                          filter(!str_detect(ngram, "\\b(\\w+)\\b\\s+\\b\\1\\b"),
                                 !str_detect(ngram, "\\b(\\w+)\\s+(\\w+)\\s+\\1\\b")))) %>% 
  select(-postags)

saveRDS(fomc_text_pos, 'data/fomc_tokens.rds')
rm(fomc_text_pos)
gc()