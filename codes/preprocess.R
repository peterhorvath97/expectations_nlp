fomc_text_raw <- read_rds('data/fomc_text_raw.rds')

prep <- function(txt){
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
  
  gc()
  txt
}

fomc_text_raw <- fomc_text_raw %>% 
  mutate(text = map(text, ~prep(.x)))

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
    filter(!(upos %in% c('X', 'PUNCT')))
    #filter(upos %in% c('NOUN', 'NUM', 'VERB', 'ADJ'))
  
  gc()
  
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
 
  
  gc()
  
  
  txt
  
}
txclean <- function(txt){
  txt <- txt %>% 
    mutate(token = tolower(token)) %>%
    group_by(doc_id) %>% 
    summarize(value = paste0(token, collapse = ' ')) %>% 
    mutate(value = removeNumbers(value),
           value = removePunctuation(value),
           value = stripWhitespace(value) %>% 
             str_remove_all('^ ') %>% 
             str_remove_all(' $'),
           value = str_replace_all(value, "\\b(\\w+)\\b\\s+\\b\\1\\b", '\\1'),
           value = str_replace_all(value, or('\\bthe', '\\bfew', '\\bof(?!f)', 'of\\b', 'the\\b', 
                                             '\\band', 'and\\b'), ''),
           value = str_replace_all(value, '\\bnei\\b', 'monei'),
           value = str_replace_all(value, '\\bm\\b \\bb\\b', 'mb'))
  
 gc()
 
 txt
}

fomc_text_raw <- fomc_text_raw %>% 
  mutate(text = map(text, ~postag(.x, udmodel)),
         postags = map(text, ~poslib(.x)),
         text = map(text, ~txclean(.x)))

saveRDS(fomc_text_raw, 'data/fomc_text_pos.rds')
rm(fomc_text_raw, udmodel)
gc()