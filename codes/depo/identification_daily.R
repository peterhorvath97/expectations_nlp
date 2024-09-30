ffr_daily <- fredr('DFF') %>% 
  select(date, value) 

topic <- read_rds('data/topic_dist_sent.rds') %>% 
  group_by(mtg_date, name, topic) %>% 
  summarize(dist = mean(dist),
            sent = mean(sent)) %>% 
  ungroup() %>% 
  rename(date = mtg_date)


topic <- topic %>% 
  filter(name %in% c('ColorBook', 'Transcript')) %>% 
  select(-dist) %>% 
  spread(name, sent) %>% 
  drop_na() %>% 
  mutate(x = Transcript-ColorBook) 

  
topic %>% 
  ggplot(aes(x = date, y = x)) +
  geom_line() +
  facet_wrap(~topic, scales = 'free') +
  theme_minimal()
  
topic <- topic %>%
  mutate(topic = case_when(topic == 'Banks' ~ 'banks',
                           topic == 'Commodity Prices' ~ 'commod',
                           topic == 'Consumer Confidence' ~ 'confid',
                           topic == 'Domestic Businesses' ~ 'busi',
                           topic == 'Exchange Rates' ~ 'exch',
                           topic == 'Financial Markets' ~ 'finmark',
                           topic == 'Financial Stability' ~ 'finstab',
                           topic == 'Fiscal Policy' ~ 'fiscal',
                           topic == 'GDP Growth' ~ 'gdp',
                           topic == 'Housing Market' ~ 'housing',
                           topic == 'Inflation' ~ 'inflat',
                           topic == 'International Trade' ~ 'trade',
                           topic == 'Labor Market' ~ 'labmark',
                           topic == 'Monetary Policy' ~ 'monpol',
                           topic == 'Money Market' ~ 'monmark',
                           topic == 'Open Market Operations' ~ 'oper',
                           topic == 'Policy Rate' ~ 'ffr',
                           topic == 'Recessions' ~ 'recess',
                           topic == 'Yield Curve' ~ 'yield')) %>% 
  select(date, topic, x) %>% 
  spread(topic, x)
  

pca <- princomp(topic[,2:ncol(topic)] %>% 
         scale()) 
summary(pca)

topic <- topic %>% 
  mutate(PC1 = predict(pca, newdata = topic)[,1])  


ffr_daily <- ffr_daily %>% 
  filter(date >= min(topic$date),
         date <= max(topic$date))


data <- ffr_daily %>% 
  full_join(topic) %>% 
  mutate(across(c(-date, -value), ~replace_na(.x, 0)))



#data %>% 
#  select(value, PC1) %>% 
#  ts() %>% 
#  vars::VARselect(lag.max = 90)


varpc <- data %>% 
  select(value, PC1) %>% 
  ts() %>% 
  vars::VAR(p = 15, type = 'none')



#data %>% 
#  select(-date, -PC1) %>% 
#  ts() %>% 
#  vars::VARselect(lag.max = 90)


varall <- data %>% 
  select(-date, -PC1) %>% 
  ts() %>% 
  vars::VAR(p = 1, type = 'none')


source('codes/varhd.R')


hdpc <- VARhd(varpc)

hdpc <- hdpc[,,1] %>% 
  as_tibble() %>% 
  bind_cols(data %>% 
              select(date))

names(hdpc)[1:2] <- c('ffr_own', 'PC1')

hdpc %>% 
  gather(key,value,-date) %>% 
  ggplot(aes(x = date, y = value, fill = key, color = key)) +
  geom_col()


hdall <- VARhd(varall)

hdall <- hdall[,,1] %>% 
  as_tibble() %>% 
  bind_cols(data %>% 
              select(date))

names(hdall) <-c('ffr_own',names(data)[3:21],'date')

hdall %>% 
  gather(key, value, -date) %>% 
  ggplot(aes(x = date, y = value, fill = key, color = key)) +
  geom_col()

shocks <- bind_cols(
  data %>% 
    select(date),
  hdall %>% 
    select(-ffr_own, -date) %>% 
    mutate(total = banks+busi+commod+
             confid+exch+ffr+finmark+finstab+
             fiscal+gdp+housing+inflat+labmark+
             monmark+monpol+oper+recess+trade+yield),
  hdpc %>% 
    select(-ffr_own, -date)
)

saveRDS(shocks, 'data/shocks.rds')
rm(ffr_daily, topic, pca, hdall, hdpc, varall, varpc, 
   companionmatrix, VARhd, VARmakexy, shocks, data)
