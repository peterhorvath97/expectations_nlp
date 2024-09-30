fomc_tokens <- read_rds('data/fomc_tokens_filtered.rds')

transcripts <- fomc_tokens %>% 
  filter(name == 'Transcript')

transcripts <- transcripts %>% 
  mutate(tokens = map(tokens,
                      ~.x %>% 
                        arrange(doc_id) %>% 
                        mutate(doc_id = as.factor(doc_id),
                               doc_id = as.numeric(doc_id))),
         doc_corr = map(tokens, ~max(.x$doc_id)) %>% unlist() %>% 
           accumulate(sum, .dir = 'forward') %>% 
           lag %>% 
           replace_na(0)) %>% 
  unnest(tokens) %>% 
  mutate(doc_id = doc_id + doc_corr) %>% 
  select(doc_id, ngram, n) 

gc()


dtm <- transcripts %>% 
  cast_dtm(document = doc_id, term = ngram, value = n)
rm(transcripts)
gc()





kmax <- 40
perplexity <- matrix(nrow = kmax, ncol = 2)
coherence <- matrix(nrow = kmax, ncol = 2)
loglik <- matrix(nrow = kmax, ncol = 2)
for(k in 2:kmax){
  print(paste("Calculating model diagnostics for ", k ," topics", sep = ""))  
  perplexity[k, 1] <- k
  coherence[k, 1] <- k
  loglik[k, 1] <- k
  
  lda <- LDA(dtm, k = k, 
             method = "Gibbs", 
             control = list(alpha = 50/k, delta = 0.025, seed = 2024, best = TRUE, 
                            burnin = 10, iter = 100, verbose = 10))
  
  perplexity[k, 2] <- perplexity(lda, dtm)
  coherence[k, 2] <- mean(topic_coherence(lda, dtm, top_n_tokens = 5))
  loglik[k, 2] <- lda@loglikelihood
  
  gc()
  
}
colnames(perplexity) <- c("k", "perplexity")
colnames(coherence) <- c("k", "coherence")
colnames(loglik) <- c('k', 'loglik')
perplexity <- as_tibble(perplexity)
coherence <- as_tibble(coherence)
diag <- bind_cols(perplexity, coherence, loglik) %>%
  select(-starts_with('k')) %>% 
  mutate(k = row_number()) %>% 
  gather(key = score, value = value,
         -k)


top500 <- dtm %>% 
  tidy() %>% 
  group_by(term) %>% 
  summarize(count = sum(count)) %>% 
  arrange(desc(count)) %>% 
  top_n(500) %>% 
  pull(term)

source('codes/seed_data.R')

seedwords <- seed_data %>% 
  pull(seedwords) %>% 
  unlist()

seed_data %>% 
  mutate(seedwords = map(seedwords, ~.x %>% 
                           as_tibble() %>% 
                           group_by(value) %>% 
                           count())) %>% 
  unnest(seedwords) %>% 
  filter(n > 1)

top500[!(top500 %in% seedwords)]


i <- seed_data %>% 
  mutate(topic_num = row_number(),
         terms = map(seedwords, ~.x %>% length()) %>% unlist(),
         i = map2(topic_num, terms, ~rep(.x, times = .y))) %>% 
  pull(i) %>% 
  unlist()

#i <- rep(1:length(seedwords), each = 1)



j <- NULL
for(z in seq_along(seedwords)){
  j[z] <- which(dtm$dimnames$Terms %in% seedwords[z]) 
}


seedwords[z]
dtm %>% 
  tidy() %>% 
  group_by(term) %>% 
  summarize(count = sum(count)) %>% 
  arrange(desc(count)) %>% 
  filter(str_detect(term, 'market condit')) 




#weight <- 1
#v = rep(weight, length(i))
v <- left_join(
  seedwords %>% 
    as_tibble() %>% 
    rename(term = value),
  dtm %>% 
    tidy() %>% 
    mutate(document = as.numeric(document),
           D = max(document)) %>% 
    group_by(term) %>% 
    mutate(tf = 1 + log(sum(count)),
           idf = log(D/n()),
           tf_idf = tf*idf) %>% 
    summarize(n = sum(count),
              navg = mean(count),
              tf = mean(tf),
              idf = mean(idf),
              tf_idf = mean(tf_idf))
) %>% 
  pull(tf_idf)

v <- v/v

kopt <- nrow(seed_data)
deltaS <- simple_triplet_matrix(i, j, v,
                                nrow = kopt, ncol = ncol(dtm))
lda <- LDA(dtm, k = kopt, 
           method = "Gibbs", 
           seedwords = deltaS,
           control = list(alpha = 50/kopt, delta = 0.025, seed = 2024, best = TRUE, 
                          burnin = 10, iter = 100, verbose = 10))


perp <- perplexity(lda, dtm)
coh <- mean(topic_coherence(lda, dtm, top_n_tokens = 5))

modeldiag <- diag %>% 
  mutate(lda = 'unsupervised') %>% 
  filter(score != 'loglik') %>% 
  mutate(score = ifelse(score == 'perplexity', 'Perplexity', 'Coherence')) %>% 
  ggplot(aes(x = k, y = value, color = lda)) +
  geom_line() +
  geom_point(data = tibble(k = c(kopt,kopt),
                           score = c('Perplexity', 'Coherence'),
                           value = c(perp, coh)) %>% 
               mutate(lda = 'seeded'),
             aes(x = k, y = value, color = lda),
             size = 2) +
  facet_wrap(~score, scales = 'free') + 
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  xlab('Number of topics') +
  ylab('') +
  scale_color_manual(values = c('darkblue', 'red'),
                     breaks = c('unsupervised', 'seeded'))
modeldiag

source('codes/lda_labels.R')

saveRDS(lda, 'data/lda.rds')
saveRDS(modeldiag, 'graphs/modeldiag.rds')
rm(coherence, perplexity, diag, dtm, fomc_tokens, lda, k, kmax, kopt, deltaS, loglik, i, j, v, seedwords, z,
   perp, coh, modeldiag, top10, top500, seed_data, labels)
gc()
