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
  select(-sent) %>% 
  spread(name, dist) %>% 
  drop_na()

lm(Transcript ~ -1 + topic + ColorBook + topic*ColorBook, 
   data = topic) %>% 
  summary()


topic <- topic %>% 
  mutate(x = lm(Transcript ~ -1 + topic + ColorBook, 
                data = .) %>% 
           resid()) 

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



topic <- topic %>% 
  mutate(year = year(date), 
         month = month(date),
         date = paste(year, 
                      ifelse(nchar(month) == 1, paste(0, month, sep = ''), month),
                      '01',
                      sep = '-') %>% 
           as_date()) %>% 
  select(-year, -month)


ffr <- ffr %>% 
  mutate(ip = ip-lag(ip),
         p = p-lag(p),
         lagval = lag(value)) %>% 
  filter(date >= min(topic$date),
         date <= max(topic$date))


data <- ffr %>% 
  full_join(topic) %>% 
  mutate(across(c(-date, -value, -p, -ip), ~replace_na(.x, 0)))

 

lm(value ~ -1 + lagval + lag(ip) + lag(p) + ffr + confid + supp + inflat + commod + labmark, 
   data = data) %>% 
  summary()
lm(value-lagval ~ -1 + lagval + lag(ip) + lag(p) + ffr + confid + supp + inflat + commod + labmark, 
   data = data) %>% 
  summary()



m1 <- lm(value ~ -1 + lagval + lag(ip) + lag(p) + ffr + confid + supp + inflat + commod + labmark, 
   data = data) %>% 
  broom::tidy() %>% 
  filter(term %in% c('ffr', 'confid', 'supp', 'inflat', 'commod', 'labmark')) %>% 
  select(estimate) %>% 
  as.matrix() 
m2 <- data %>% 
  gather(key = name, 
         value = value, 
         -date) %>% 
  filter(name %in% c('ffr', 'confid', 'supp', 'inflat', 'commod', 'labmark')) %>% 
  spread(name, value) %>% 
  select(ffr, confid, supp, inflat, commod, labmark) %>% 
  as.matrix()

shock1 <- m1[1]*m2[,1] + m1[2]*m2[,2]  + m1[3]*m2[,3]  + m1[4]*m2[,4]  + m1[5]*m2[,5]  + m1[6]*m2[,6]  



m1 <- lm(value ~ -1 +
     lag(value, 1) + lag(value, 2) + lag(value, 3) + lag(value, 4) + lag(value, 5) + lag(value, 6) + 
     lag(value, 7) + lag(value, 8) + lag(value, 9) + lag(value, 10) + lag(value, 11) + lag(value, 12) +
     lag(ip, 1) + lag(ip, 2) + lag(ip, 3) + lag(ip, 4) + lag(ip, 5) + lag(ip, 6) + 
     lag(ip, 7) + lag(ip, 8) + lag(ip, 9) + lag(ip, 10) + lag(ip, 11) + lag(ip, 12) +
     lag(p, 1) + lag(p, 2) + lag(p, 3) + lag(p, 4) + lag(p, 5) + lag(p, 6) + 
     lag(p, 7) + lag(p, 8) + lag(p, 9) + lag(p, 10) + lag(p, 11) + lag(p, 12) +
     ffr + confid + supp + inflat + commod + labmark,
   data = data) %>% 
  broom::tidy() %>% 
  filter(term %in% c('ffr', 'confid', 'supp', 'inflat', 'commod', 'labmark')) %>% 
  select(estimate) %>% 
  as.matrix() 
shock2 <- m1[1]*m2[,1] + m1[2]*m2[,2]  + m1[3]*m2[,3]  + m1[4]*m2[,4]  + m1[5]*m2[,5]  + m1[6]*m2[,6]  

shock <- bind_cols(data$date, shock1, shock2) %>% 
  rename(date = ...1,
         shock1 = ...2,
         shock2 = ...3) 

shock%>% 
  gather(key, value, -date) %>% 
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line()

rm(data, ffr, ip, p, m1, m2, p, topic, shock1, shock2)

fred_query <- function(ids, freq, start){
  
  require(fredr)
  require(dplyr)
  require(purrr)
  
  #download data
  params <- list(
    series_id = ids,
    frequency = freq,
    observation_start = as.Date(start)
  )
  
  
  data  <- pmap_dfr(
    .l = params,
    .f = ~ fredr(series_id = .x, frequency = .y) ) %>%
    dplyr::select(date, series_id, value) %>%
    spread(key = series_id, value = value) %>%
    drop_na() 
  
  data
}

fred <- fred_query(ids = c(
  #Straight forward ones
  'PCEPI','INDPRO', 'BUSLOANS', 'M1SL', 'FEDFUNDS', 'PPIACO',
  #Household Loans
  'CONSUMER', 'REALLN',
  #TED spread
  'IR3TED01USM156N', 'TB3MS',
  #Bond spread
  'AAA', 'BAA',
  #Yield Curve
  'GS10'),
  freq = 'm',  
  start = '1950-01-01')


fred <- fred %>% 
  mutate(TED = IR3TED01USM156N-TB3MS,
         HL = CONSUMER + REALLN,
         BS = BAA-AAA,
         YC = GS10-TB3MS) %>% 
  select(-IR3TED01USM156N, -TB3MS,
         -CONSUMER, -REALLN,
         -BAA, -AAA,
         -GS10) %>% 
  rename(CP = PPIACO,
         BL = BUSLOANS,
         R = FEDFUNDS,
         P = PCEPI,
         IP = INDPRO,
         M = M1SL) %>% 
  mutate(BL = log(BL),
         HL = log(HL),
         M = log(M))




data <- shock %>% 
  inner_join(fred) 

ydata <- data %>% 
  select(R, M, YC, BS, TED, BL, HL, P, IP) %>% 
  as.matrix()

z1 <- data %>% 
  select(shock1) %>%
  pull(shock1) %>% 
  as.numeric()

z2 <- data %>% 
  select(shock2) %>%
  pull(shock2) %>% 
  as.numeric()


VAR1 <- varexternal::SVARIV(ydata = ydata,
                            z = z1,
                            p = 12, 
                            NWlags = 0,
                            norm = 1, 
                            scale = 1,
                            horizons = 60,
                            confidence = c(0.84,0.95),
                            instrument_name = 'instrument')


VAR2 <- varexternal::SVARIV(ydata = ydata,
                            z = z2,
                            p = 12, 
                            NWlags = 0,
                            norm = 1, 
                            scale = 1,
                            horizons = 60,
                            confidence = c(0.84,0.95),
                            instrument_name = 'instrument')


svariv2 <- list(VAR1, VAR2)
names(svariv2) <- c('taylor', 'var')
saveRDS(svariv2, 'data/svariv2.rds')

VAR2$irfs %>% 
  filter(confi_level == 0.84) %>% 
  spread(type, value) %>% 
  filter(confi_type == 'plugin') %>% 
  ggplot(aes(x = horizon, y = point,
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_ribbon(alpha = .2) +
  geom_hline(color = 'red', 
             linetype = 'dashed',
             yintercept = 0) +
  facet_wrap(~variable, scales = 'free')


VAR1$waldstat %>% 
  filter(type == 'Waldstat',
         level ==0.95)
