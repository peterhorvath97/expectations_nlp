fomc_tokens <- read_rds('data/fomc_tokens.rds') 



transcripts <- fomc_tokens %>% 
  filter(name == 'Transcript') %>% 
  mutate(doc_corr = map(tokens, ~max(.x$doc_id)) %>% unlist() %>% 
           accumulate(sum, .dir = 'forward') %>% 
           lag %>% 
           replace_na(0)) %>% 
  unnest(tokens) %>% 
  mutate(doc_id = doc_id + doc_corr) %>% 
  select(doc_id, ngram, n) %>% 
  filter(nchar(ngram) > 1)


  

unigrams <- transcripts %>% 
  distinct(ngram) %>% 
  filter(!str_detect(ngram, ' ')) %>% 
  filter((ngram %in% c('inflat', 'sheet', 'home', 'rate', 'growth', 'guidanc', 'market', 'polici', 'price', 
                       'term', 'economi', 'risk', 'fund', 'effect', 'econom', 'repusrchase', 'forecast', 'statement',
                       'real', 'interest', 'increas', 'forecast', 'expect', 'bank', 'busi', 'target', 'product',
                       'dollar', 'balance', 'labor', 'outlook', 'declin', 'employ', 'monei', 'sale', 'credit',
                       'asset', 'gdp', 'cost', 'reserv', 'uncertainti', 'hous', 'spend', 'consum', 'treasuri',
                       'invest', 'trend', 'estim', 'wage', 'oil', 'job', 'core', 'debt', 'recoveri', 'output',
                       'develop', 'countri', 'recess', 'suppli', 'secur', 'spread', 'incom', 'aggreg', 'capit',
                       'rise', 'adjust', 'inventori', 'yield', 'energi', 'expans', 'manufactur', 'state', 'stock',
                       'industri', 'world', 'trade', 'option', 'loan', 'downsid', 'curv', 'index', 'shock', 'work',
                       'export', 'retail', 'liquid', 'gap', 'tax', 'mortgag', 'govern', 'household', 'firm', 'govern',
                       'exchang', 'pce', 'bond', 'equiti', 'currenc', 'reduct', 'revis', 'investor', 'volatil',
                       'tighten', 'consumpt', 'good', 'gnp', 'cpi', 'borrow', 'japan', 'swap', 'deficit', 'indic',
                       'commod', 'crisi', 'cut', 'premium', 'cycl', 'portfolio', 'slack', 'fall', 'stimulu', 'stabil',
                       'profit', 'deposit', 'ratio', 'auto', 'financ', 'euro', 'unemploy', 'us', 'mb', 'job', 'system',
                       'acceler', 'slowdown', 'downward', 'intervent', 'threshold', 'china', 'liabil',
                       'sentiment', 'cap', 'tealbook', 'mandat', 'japanes', 'deflat', 'repo', 'budget', 'momentum',
                       'lend', 'stress', 'banker', 'constraint', 'yen', 'mexico', 'upside', 'packag', 'equilibrium',
                       'expenditur', 'peak', 'ecb', 'food', 'vehicl', 'steel', 'deterior', 'depreci', 'appreci',
                       'auction', 'fundament', 'road', 'rebound', 'law', 'ga', 'tech', 'counterparti', 'shortfal',
                       'truck', 'elect', 'downturn', 'canada', 'famili', 'strike', 'taper', 'rais', 'properti',
                       'disinfl', 'reform', 'motor', 'default', 'legisl', 'barrel', 'vulner', 'defens', 'tradeoff',
                       'germani', 'anticip', 'hike', 'regul', 'corpor', 'benchmark', 'offset', 'employe', 'spillov',
                       'bubbl', 'demand', 'coupon', 'war', 'brazil', 'turmoil', 'transitori', 'tension', 'layoff',
                       'gasolin', 'tariff', 'fragil', 'greec', 'cd', 'aggricultur', 'supplier', 'durabl', 'specul',
                       'surplu', 'refinanc', 'breakeven', 'tourism', 'transmiss', 'restructur', 'greenbook', 
                       'arbitrag', 'bankrupci', 'subprim', 'nairu', 'pension', 'minimum', 'ship', 'salari', 'metal',
                       'itali', 'buffer', 'spain', 'puzzl', 'transfer', 'gold', 'grain', 'turbul', 'dispers', 
                       'regress', 'corn', 'owner', 'linkag', 'corridor', 'fee', 'restrict', 'ppi', 'travel', 'capac',
                       'iraq', 'peso', 'attack', 'warn', 'automobil', 'crise', 'italian', 'sluggish', 'wheat',
                       'trend', 'brazilian', 'slump', 'cattl', 'flood', 'libor', 'machineri', 'russia', 'nonbank',
                       'interbank', 'brexit', 'chip', 'haircut', 'homeown', 'refin', 'sell', 'port', 'india', 'tool',
                       'saver', 'barrier', 'global', 'retail', 'bundesbank', 'macroeconomi', 'aluminum', 'bust',
                       'copper', 'surplus', 'creditor', 'billion', 'forward', 'swiss', 'portug', 'turkei', 'shift',
                       'iran', 'australia', 'gulf', 'foreign', 'migrat', 'meat', 'petroleum', 'downshift', 'impact',
                       'refineri', 'contractionari', 'spiral', 'homebuy', 'taiwan', 'circul', 'stimul', 'event',
                       'nearterm', 'deutschemark', 'afford', 'riski', 'livestock', 'nondefens', 'cdo', 'industri',
                       'telecom', 'taxpay', 'sweden', 'terrorist', 'mandat', 'nonfuel', 'singapor', 'account', 'import',
                       'safeguard', 'nonmanufactur', 'globe', 'tailwind', 'bluebook', 'georgia', 'destruct', 'tighten',
                       'eurodollar', 'venezuela', 'oecd', 'mill', 'defici', 'buyout', 'deregul', 'shrinkag', 'net',
                       'shortrun', 'german', 'harvest', 'quota', 'outbreak', 'sticki', 'chicken', 'paycheck', 'public',
                       'ukrain', 'deflationari', 'bullish', 'longrun', 'hawk', 'wholesal', 'indonesia', 'redbook',
                       'capita', 'illiquid', 'solvenc', 'fdic', 'jgb', 'subsidiari', 'discretionari', 'fed',
                       'bull', 'gross', 'gallon', 'treasur', 'silver', 'suburb', 'bailout', 'boe', 'cotton', 'averag',
                       'underperform', 'overreact', 'terror', 'stagflat', 'groceri', 'child', 'censu', 'activ',
                       'disequilibrium', 'tank', 'prudenc', 'fish', 'australian', 'financi', 'debit', 'pressur',
                       'defer', 'fiscal', 'monetari', 'nber', 'beef', 'weapon', 'homeownership', 'tobacco', 'lump',
                       'petrochem', 'cyclic', 'feder', 'cashflow', 'ev', 'cargo', 'upcreep', 'nigeria', 'estim',
                       'improv', 'leverag', 'quantit', 'eas', 'rrp', 'ioer')))





vocab <- transcripts %>% 
  distinct(ngram) 

vocab <- vocab %>% 
  filter(!str_detect(ngram, rebus::or('\\bbit\\b', '\\bmani', 'peopl', 'lunch', 'exampl', 'lot', 'wai',
                                      '^\\btime\\b ', '\\btime\\b$', 'reason', 'idea', 'much', '\\bpart\\b',
                                      'chair', 'thing', 'divis', 'director', '\\bb\\b', 'year', 'adjust season',
                                      'quarter', 'month$', '^month', 'that', 'like', 'right', 'left', 'ii',
                                      '\\b[a-z]\\b', 'chart', '\\bab\\b', 'question', 'meet', 'affair monetari',
                                      '\\babil\\b', 'about', 'morn', 'item', '\\bdium\\b', 'percent', '^work$',
                                      '^good ', 'deal good', 'comment', 'option', '^reserv$', '^system$',
                                      'term', 'annual rate', 'break coffe', 'paragraph', '^outlook$'
  ))) %>% 
  filter(str_detect(ngram, paste('\\b', pull(unigrams, ngram), '\\b', sep = '') %>% 
                      paste(collapse = '|')
  )) %>% 
  bind_rows(tibble(ngram = c('good servic', 'term spread', 'term premium', 'term rrp',
                             'option stock', 'option price')))



vocab <- vocab %>%
  mutate(id = row_number()) %>%
  separate_rows(ngram, sep = ' ') %>% 
  mutate(nchar = nchar(ngram)) %>% 
  group_by(id) %>% 
  mutate(maxnchar = max(nchar),
         minnchar = min(nchar)) %>% 
  summarize(ngram = paste(ngram, collapse = ' '),
            maxnchar = mean(maxnchar),
            minnchar = mean(minnchar)) %>% 
  select(-id) %>% 
  filter(maxnchar <= 10 | maxnchar > 10 & str_detect(ngram,c('inflationari',
                                                       'residenti',
                                                       'nonresidenti',
                                                       'multifactor',
                                                       'contemporan',
                                                       'relationship',
                                                       'satisfactori',
                                                       'expansionari',
                                                       'creditworthi',
                                                       'extraordinari',
                                                       'macroprudenti',
                                                       'disinflationari',
                                                       'nonrecessionari',
                                                       'noninflationari',
                                                       unigrams %>% 
                                                         select(ngram) %>% 
                                                         mutate(nchar = nchar(ngram)) %>% 
                                                         arrange(desc(nchar)) %>% 
                                                         filter(nchar > 10) %>% 
                                                         pull(ngram)) %>% 
                                                 paste(collapse = '|') )) %>%
  filter(minnchar > 1) %>% 
  select(-minnchar, -maxnchar)

rm(unigrams)


vocab <- transcripts %>% 
  inner_join(vocab) %>% 
  group_by(ngram) %>% 
  summarize(n = sum(n))

unigrams <- vocab %>% 
  filter(str_count(ngram, ' ') == 0)

bigrams <- vocab %>% 
  filter(str_count(ngram, ' ') == 1)

trigrams <- vocab %>% 
  filter(str_count(ngram, ' ') == 2)


bigrams <- bigrams %>% 
  mutate(q = quantile(n, .95)) %>% 
  filter(n >= q) %>% 
  arrange(n)

trigrams <- trigrams %>% 
  mutate(q = quantile(n, .95)) %>% 
  filter(n >= q) %>% 
  arrange(n)

unigrams <- unigrams %>% 
  mutate(q1 = quantile(n, .05) %>% round(-1), #under 30
         q2 = quantile(n, .95) %>% round(-3) #over 10000
         ) %>% 
  filter(n >= q1,
         n <= q2) %>% 
  arrange(n %>% desc) 

#unigrams <- unigrams %>% 
#  filter(n > quantile(n, .05))
bigrams <- bigrams %>% 
  filter(n >= 20)
trigrams <- trigrams %>% 
  filter(n >= 10)

vocab <- bind_rows(unigrams, bigrams, trigrams) %>% 
  select(ngram)


tfidf <- function(df){
  df <- df %>% 
    mutate(D = max(doc_id)) %>% 
    group_by(ngram) %>% 
    mutate(tf = 1+ log(sum(n)),
           idf = log(D/n()),
           tf_idf = tf*idf,
           n = sum(n)) %>%   
    select(-D) %>% 
    ungroup() %>% 
    arrange(desc(tf_idf)) %>% 
    distinct(ngram, .keep_all = T)
  
  df
}

vocab <- transcripts %>% 
  inner_join(vocab) %>% 
  filter(str_detect(ngram, ' ')) %>% 
  mutate(doc_id = as.factor(doc_id),
         doc_id = as.numeric(doc_id)) %>% 
  tfidf() %>% 
  mutate(rank = row_number())


plot <- vocab %>% 
  ggplot(aes(x = rank, y = tf_idf)) +
  geom_line() +
  #geom_hline(aes(yintercept = 20), color = 'red', linetype = 'dashed') +
  geom_vline(aes(xintercept = 7000), color = 'red', linetype = 'dashed') +  
  theme_minimal() +
  xlab('Rank') +
  ylab('TF-IDF score')
#plot
saveRDS(plot, 'graphs/tfidfplot.rds')

vocab <- vocab %>% 
  #filter(tf_idf >= 20) %>%
  filter(rank <= 7000) %>% 
  select(ngram) %>% 
  bind_rows(unigrams %>% select(ngram)) 


fomc_tokens <- fomc_tokens %>% 
  unnest(tokens) %>% 
  mutate(tag = ifelse(ngram == 'uncertainti', 1, 0)) %>% 
  group_by_at(vars(-ngram, -n,-tag)) %>% 
  mutate(tag = max(tag)) %>% 
  ungroup() %>% 
  inner_join(vocab) %>% 
  group_by_at(vars(-doc_id, -ngram, -n, -tag)) %>% 
  mutate(doc_id = as.factor(doc_id), 
         doc_id = as.numeric(doc_id)) %>% 
  nest() %>% 
  mutate(tokens = map(data, 
                      ~.x %>% 
                        select(-tag))) %>% 
  rename(tagged = data) %>% 
  ungroup()


saveRDS(fomc_tokens, 'data/fomc_tokens_filtered.rds')
rm(unigrams, bigrams, trigrams, transcripts, fomc_tokens, vocab, tfidf, plot)
gc()




