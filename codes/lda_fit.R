fomc_tokens <- read_rds('data/fomc_tokens_filtered.rds')

transcripts <- fomc_tokens %>% 
  filter(name == 'Transcript')

transcripts <- transcripts %>% 
  mutate(doc_corr = map(tokens, ~max(.x$doc_id)) %>% unlist() %>% 
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
                            burnin = 100, iter = 300, verbose = 10))
  
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

modeldiag <- diag %>% 
  mutate(score = ifelse(score == 'perplexity', 'Perplexity', score),
         score = ifelse(score == 'coherence', 'Coherence', score),
         score = ifelse(score == 'loglik', 'Log-likelihood', score)) %>% 
  ggplot(aes(x = k, y = value)) +
  geom_line() +
  facet_wrap(~score, scales = 'free') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  xlab('Number of topics') +
  ylab('')
saveRDS(modeldiag, 'graphs/modeldiag.rds')

read_rds('data/modeldiag.rds')%>% 
  mutate(score = ifelse(score == 'perplexity', 'Perplexity', score),
         score = ifelse(score == 'coherence', 'Coherence', score),
         score = ifelse(score == 'loglik', 'Log-likelihood', score)) %>% 
  ggplot(aes(x = k, y = value)) +
  geom_line() +
  facet_wrap(~score, scales = 'free') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  xlab('Number of topics') +
  ylab('')

diag %>% 
  spread(score, value) %>% 
  filter(k > 50) %>% 
  print(n = 100)




lda <- LDA(dtm, k = 40, 
           method = "Gibbs", 
           control = list(alpha = 50/40, delta = 0.025, seed = 2024, best = TRUE, 
                          burnin = 100, iter = 300, verbose = 10))


labels <- tibble(topic = c(1:40),
            theme = c('Wages',
                      'Monetary policy',
                      'Exchange rates',
                      'Fiscal Policy',
                      'Mandate',
                      'Securities',
                      'Forward guidance',
                      'Economic outlook',
                      'Money market',
                      'Real estate',
                      'Oil price',
                      'Industrial production',
                      'Supply',
                      'Policy rate',
                      'Risks',
                      'Uncertainty',
                      'Inflation',
                      'Interest rates',
                      'World economy',
                      'Monetary aggregates',
                      'Consumption',
                      'Financial stability',
                      'Long run trends',
                      'Supply shocks',
                      'Stock market',
                      'Agricultural prices',
                      'Business cycle',
                      'Trade',
                      'Investments',
                      'Commodity prices',
                      'QE',
                      'Gov debt',
                      'Credit markets',
                      'Recessions',
                      'Labor market',
                      'Employment',
                      'Balance sheet',
                      'GDP',
                      'Foreign economies',
                      'Productivity'))
saveRDS(labels, 'data/labels.rds')



terms(lda, 10)

i = 40
wordcloud::wordcloud(words = tidy(lda, matrix = 'beta') %>% 
                       filter(topic == i) %>% 
                       pull(term),
                     freq = tidy(lda, matrix = 'beta') %>% 
                       filter(topic==i) %>% 
                       pull(beta) %>% 
                       sqrt(),
                     max.words = 100,
                     random.order = FALSE,
                     rot.per = 0,
                     main = 'valami')




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

saveRDS(lda, 'data/lda.rds')
rm(coherence, perplexity, diag, dtm, fomc_tokens, lda, k, kmax, deltaS, loglik, i,
   perp, coh, modeldiag, top10, labels)
gc()
