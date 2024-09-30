fomc_text_raw <- read_rds('data/fomc_text_raw.rds')

fomc_text_raw <- fomc_text_raw %>% 
  filter(name == 'Transcript') %>% 
  slice(1:10)
  filter(mtg_date == max(mtg_date))

declutter <- function(txt){
  patterns <- c("\\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2}(?:–\\d{1,2})?,\\s+\\d{4}\\b",
                "\\b(?:Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday)",
                or('a.m.', 'p.m.'),
                one_or_more(DGT) %R% ' of ' %R% one_or_more(DGT))
  
  txt <- txt %>% 
    mutate(value = str_conv(value, 'UTF8') %>% 
             str_remove_all(paste0(patterns, collapse = '|'))) %>% 
    separate_rows(value, sep = '\\n\\n\\n\\n') %>% 
    mutate(value = str_remove_all(value, one_or_more('\n')) %>% 
             stripWhitespace() %>% 
             str_replace_all(DOT, DOT %R% ' ')) %>% 
    filter(value != ' ') %>% 
    mutate(value = value %>% 
             str_remove_all(START %R% SPACE) %>% 
             str_remove_all(SPACE %R% END))
  
  txt
}

fomc_text_raw <- fomc_text_raw %>% 
  mutate(text = map(text, ~declutter(.x)))

udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)


postag <- function(txt, udmodel){
  txtpos <- udpipe_annotate(udmodel, txt$value, trace = TRUE)
  
  txtpos <- txtpos %>% 
    as_tibble() %>% 
    select(doc_id, token, upos) %>% 
    mutate(upos = ifelse(str_detect(token %>% 
                                      tolower %>% 
                                      stem_strings, 
                                    countrycode::codelist %>% 
                                      pull(country.name.en) %>% 
                                      tolower() %>% 
                                      stem_strings() %>% 
                                      paste0(collapse = '|')), # & upos == 'PROPN',
                         'NOUN',
                         upos),
           upos = ifelse(str_detect(token %>% 
                                      tolower %>% 
                                      stem_strings,
                                    c('^dollar$', '^euro$', '^yen$', '^pound$', '^peso$', '^krona$', '^franc$', '^ruble$', 
                                      '^dinar$', '^won$', '^rupee$', '^rand$', '^zloty$', '^dirham$', '^real$', '^yuan$',
                                      '^krone$', '^lira$', '^dirham$', '^riyal$', '^rupiah$') %>% 
                                      paste0(collapse = '|')),# & upos == 'PROPN',
                         'NOUN',
                         upos),
           upos = ifelse(str_detect(token %>% 
                                      tolower %>% 
                                      stem_strings,
                                    c('^gdp$', '^cpi$', '^pce$', '^gnp$', '^ppp$', '^fdi$', '^imf$', '^wto$', '^vat$',
                                      '^opec$', '^nafta$', '^ecb$', '^oecd$', '^m2$', '^m3$', '^gfc$', '^hicp$', '^brics$',
                                      '^tfp$') %>% 
                                      paste0(collapse = '|')), # & upos == 'PROPN',
                         'NOUN',
                         upos)) %>% 
    filter(upos %in% c('NOUN', 'NUM', 'VERB', 'ADJ'))
    
  
  txtpos
}
poslib <- function(txt){
  txt  <- txt %>% 
    mutate(token = tolower(token)) %>%
             group_by(doc_id, token, upos) %>% 
             count() %>% 
             ungroup(upos) %>% 
             filter(n == max(n)) %>% 
             select(-n) %>% 
    distinct(doc_id, token, .keep_all = T)
  txt
           
           
  
}
txclean <- function(txt){
  txt <- txt %>% 
    mutate(token = tolower(token)) %>%
    group_by(doc_id) %>% 
    summarize(value = paste0(token, collapse = ' ')) %>% 
    mutate(value = removeNumbers(value),
           value = removePunctuation(value),
           value = removeWords(value, c(stopwords('en'), "\\b\\w{1,2}\\b", "\\b\\w{1,2}\\b", 
                                        'think[s|ing]*', 'thank[s|ing]*', 'meeting[s]*', 'panel[s]*', 'slide[s]*', 'chart[s]*', 'figure[s|d]*', 'graph[s]*', 'page[s]*', 'see[s|n|ing]*',
                                        'red', 'green', 'blue', 'black', 'white', 'gray', 'grey', 'left', 'right', 'top', 'bottom', 'deput[y|ies]*',
                                        'line', 'said', 'few', 'little', 'bit', 'dash', 'break', 'hand[s]*', 'attend[s|ed]*', 'like[s|d|ly]*', 'office[s]*', 'governor[s]*',
                                        'board', 'data', 'staff', 'counsel', 'division[s]*', 'manager[s]*', 'question[s|ed|ing]*', 'minute[s]*', 'advise[s]*', 'associate[s,d]*',
                                        'discuss[ed|ing]*', 'chairm[a|e]n', 'paragraph[s]*', 'say[s|ing]*', 'thing[s]*', 'stuff', 'get[s|ing]*', 'got', 'show[s|ing|n]*',
                                        '[un]*able', 'appropriate', 'continue[s|d]*', 'chair')),
           value = stripWhitespace(value) %>% 
             str_remove_all('^ ') %>% 
             str_remove_all(' $'),
           value = str_replace_all(value, "\\b(\\w+)\\b\\s+\\b\\1\\b", '\\1'))
  
  txt
}

fomc_text_raw <- fomc_text_raw %>% 
  mutate(text = map(text, ~postag(.x, udmodel)),
         postags = map(text, ~poslib(.x)),
         text = map(text, ~txclean(.x)))


fomc_text_raw <- fomc_text_raw %>% 
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

fomc_text_raw <- fomc_text_raw %>% 
  mutate(unigram = map2(unigram, postags, ~inner_join(.x,
                                                      .y,
                              by = c('doc_id', 'ngram' = 'token')) %>% 
                          filter(upos  %in%  c('NOUN')) %>% 
                          select(-upos) %>% 
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
                         filter(upos  %in%  c('ADJ ADJ',
                                              'ADJ NOUN',
                                              'NOUN NOUN',
                                              'NUM NOUN', #kérdéses
                                              'NOUN ADJ', #szerintem nem
                                              'NOUN VERB', #kérdéses
                                              'VERB NOUN' #kérdéses
                         )) %>%
                         select(-token_id, -upos) %>% 
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
                          filter(upos %in%  c('ADJ ADJ NOUN', #maybe?
                                              'ADJ NOUN ADJ', #maybe?
                                              'ADJ NOUN NOUN',
                                              'ADJ NOUN VERB', #maybe?
                                              'ADJ NUM NOUN', #maybe?
                                              'NOUN ADJ NOUN', #maybe?
                                              'NOUN NOUN ADJ', 
                                              'NOUN NOUN NOUN',
                                              'NOUN NOUN VERB',
                                              'NOUN VERB NOUN',
                                              'NUM NOUN NOUN', #maybe?
                                              'VERB ADJ NOUN', #maybe?
                                              'VERB NOUN NOUN')) %>% 
                          select(-token_id, -upos) %>% 
                          mutate(ngram = stem_strings(ngram)) %>% 
                          group_by(doc_id, ngram) %>% 
                          count() %>% 
                          filter(!str_detect(ngram, "\\b(\\w+)\\b\\s+\\b\\1\\b"),
                                 !str_detect(ngram, "\\b(\\w+)\\s+(\\w+)\\s+\\1\\b")))) %>% 
  select(-postags)


fomc_text_raw <- fomc_text_raw %>% 
  gather(key = collocation, value = data,
         unigram, bigram, trigram) %>% 
  unnest(data) %>% 
  group_by_at(vars(-'collocation', -'doc_id', -'ngram', -'n')) %>% 
  nest()








wordfilter1 <- fomc_text_raw %>% 
  unnest(data) %>%
  ungroup() %>% 
  group_by(collocation, ngram) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(f = case_when(collocation == 'trigram' ~ 50,
                       collocation == 'bigram' ~ 100,
                       collocation == 'unigram' ~ 200)) %>% 
  filter(n > f) %>% 
  distinct(ngram) %>% 
  pull(ngram)

fomc_text_raw <- fomc_text_raw %>% 
  mutate(data = map(data, ~.x %>% 
                      filter(ngram %in% wordfilter1)))

wordfilter2 <- fomc_text_raw %>% 
  ungroup() %>% 
  select(data) %>% 
  unnest(data) %>% 
  filter(collocation %in% c('bigram', 'trigram')) %>% 
  select(ngram) %>% 
  separate_rows(ngram, sep = ' ') %>% 
  distinct() %>% 
  pull(ngram)

fomc_text_raw %>% 
  unnest(data) %>% 
  group_by_at(vars( -'doc_id', -'ngram', -'n')) %>% 
  nest() %>% 
  spread(collocation, data) %>% 
  mutate(unigram = map(unigram, ~.x %>% 
                         filter(ngram %in% wordfilter2))) %>%
  gather(key = collocation,
         value = data,
         unigram, bigram, trigram) %>%
  unnest(data) %>% 
  select(-collocation) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(data = map(data, ~.x %>% 
                      mutate(doc_id = str_remove_all(doc_id, 'doc') %>% 
                               as_factor() %>% 
                               as.numeric())
                      ),
         doc_corr = map(data, ~max(.x$doc_id)) %>% 
           unlist() %>%  
           accumulate(sum, .dir = 'forward') %>% 
           lag %>% 
           replace_na(0)
         ) %>% 
  unnest(data) %>% 
  mutate(doc_id = doc_id + doc_corr) %>% 
  select(-doc_corr)



