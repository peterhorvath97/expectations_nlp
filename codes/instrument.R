idx_df <- read_rds('data/uncertainty.rds')
#sentiment <- read_rds('data/sentiment.rds')


#idx_df <- idx_df %>% 
#  left_join(sentiment) %>% 
#  mutate(dist = dist*sentiment) %>% 
#  select(-sentiment)
#rm(sentiment)

idx_df <- idx_df %>% 
  filter(name == 'Transcript' | name == 'Greenbook') %>%
  group_by(date, name, topic) %>% 
  summarize(dist = mean(dist)) %>% 
  spread(name, dist) %>% 
  arrange(topic, date) %>% 
  ungroup() %>% 
  group_by(topic) %>% 
  mutate(Greenbook_l = lag(Greenbook))

idx_df %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = Transcript), color = 'darkblue') +
  geom_line(aes(y = Greenbook_l), color = 'darkgreen') +
  facet_wrap(~topic, scales = 'free')


idx_df <- idx_df %>% 
  drop_na()


lm(Transcript~-1+topic+Greenbook_l, data  = idx_df) %>% 
  summary()

lm(Greenbook~-1+topic+Greenbook_l, data  = idx_df) %>% 
  summary()


idx_df %>% 
  ungroup() %>% 
  mutate(shock_transcript = lm(Transcript~-1+topic+Greenbook_l, 
                               data  = .) %>% 
           resid(),
         shock_greenbook = lm(Greenbook~-1+topic+Greenbook_l, 
                              data  = idx_df) %>% 
           resid(),
         shock_diff = Transcript - Greenbook_l) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = shock_transcript), color = 'darkblue') +
  geom_line(aes(y = shock_greenbook), color = 'darkgreen') +
  geom_line(aes(y = shock_diff), color = 'black') +
  facet_wrap(~topic, scales = 'free')




shocks <- idx_df %>% 
  ungroup() %>% 
  mutate(shock_transcript = lm(Transcript~-1+topic+Greenbook_l, 
                               data  = .) %>% 
           resid(),
         shock_greenbook = lm(Greenbook~-1+topic+Greenbook_l, 
                              data  = idx_df) %>% 
           resid(),
         shock_diff = Transcript - Greenbook_l) %>% 
  select(date, topic, starts_with('shock')) %>% 
  gather(shock, value, starts_with('shock')) %>% 
  mutate(shock = str_remove_all(shock, 'shock_')) %>% 
  group_by(topic, shock) %>% 
  mutate(sd = sd(value),
         pos = ifelse(value > 2*sd, 1, 0),
         neg = ifelse(value < -2*sd, 1, 0),
         selector = pmax(pos, neg),
         value_extreme = value*selector) %>% 
  select(-sd, -pos, -neg, -selector) %>% 
  rename(base = value,
         outlier = value_extreme) %>% 
  gather(key = type, value = value, base, outlier)




shocks %>%
  filter(type == 'base') %>% 
  ggplot(aes(x = date, y = value, color = shock)) +
  geom_line() + 
  facet_wrap(~topic, scales = 'free')



saveRDS(shocks, 'data/shocks.rds')
rm(idx_df, shocks)
