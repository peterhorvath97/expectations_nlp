ffr <- fredr('FEDFUNDS') %>% 
  select(date, value) 
ip <- fredr('INDPRO') %>% 
  select(date, ip = value)
p <- fredr('PCEPI') %>% 
  select(date, p = value)

ffr <- inner_join(ffr, ip) %>% 
  inner_join(p)

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

topic <- topic %>% 
  mutate(year = year(date),
         month = month(date),
         month = ifelse(nchar(month) == 1, paste('0', month, sep = ''), as.character(month)),
         date = paste(year, month, '01', sep = '-') %>% as_date()) %>% 
  select(-year, -month) %>% 
  group_by(date, topic) %>% 
  summarize(x = mean(x)) %>% 
  ungroup()


topic %>% 
  ggplot(aes(x = date, y = x)) +
  geom_line() +
  facet_wrap(~topic, scales = 'free') +
  theme_minimal()

topic <- topic %>%
  mutate(topic = case_when(topic == 'Banks' ~ 'banks',
                           topic == 'Commodity Prices' ~ 'commod',
                           topic == 'Consumer Confidence' ~ 'confid',
                           topic == 'Domestic Supply' ~ 'supp',
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


ffr <- ffr %>% 
  filter(date >= min(topic$date),
         date <= max(topic$date))


data <- ffr %>% 
  full_join(topic) %>% 
  mutate(across(c(-date, -value, -p, -ip), ~replace_na(.x, 0)))



data %>%  
  select(value, p, ip, PC1) %>% 
  ts() %>% 
  vars::VARselect(lag.max = 12)


varpc <- data %>% 
  select(value, p, ip, PC1) %>% 
  ts() %>% 
  vars::VAR(p = 12, type = 'none')



data %>% 
  select(-date, -PC1) %>% 
  ts() %>% 
  vars::VARselect(lag.max = 12)


varall <- data %>% 
  select(-date, -PC1) %>% 
  ts() %>% 
  vars::VAR(p = 12, type = 'none')


source('codes/varhd.R')


hdpc <- VARhd(varpc)

hdpc <- hdpc[,,1] %>% 
  as_tibble() %>% 
  bind_cols(data %>% 
              select(date))

names(hdpc)[1:4] <- c('ffr_own', 'p', 'ip', 'PC1')

hdpc %>% 
  gather(key,value,-date) %>% 
  ggplot(aes(x = date, y = value, fill = key, color = key)) +
  geom_col()


hdall <- VARhd(varall)

hdall <- hdall[,,1] %>% 
  as_tibble() %>% 
  bind_cols(data %>% 
              select(date))

names(hdall) <-c('ffr_own', 'ip', 'p', names(data)[5:23],'date')

hdall %>% 
  gather(key, value, -date) %>% 
  ggplot(aes(x = date, y = value, fill = key, color = key)) +
  geom_col()

shocks <- bind_cols(
  data %>% 
    select(date),
  hdall %>% 
    select(-ffr_own,-ip,-p, -date) %>% 
    mutate(total = banks+supp+commod+
             confid+exch+ffr+finmark+finstab+
             fiscal+gdp+housing+inflat+labmark+
             monmark+monpol+oper+recess+trade+yield),
  hdpc %>% 
    select(-ffr_own,-p,-ip, -date)
)

saveRDS(shocks, 'data/shocks.rds')
rm(ffr, p, ip, topic, pca, hdall, hdpc, varall, varpc, 
   companionmatrix, VARhd, VARmakexy, shocks, data)
