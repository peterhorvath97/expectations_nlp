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
  spread(topic, x) %>% 
  mutate(year = year(date), 
         month = month(date),
         date = paste(year, 
                      ifelse(nchar(month) == 1, paste(0, month, sep = ''), month),
                      '01',
                      sep = '-') %>% 
           as_date())

topic %>% 
  gather(topic, x,-date) %>% 
  ggplot(aes(x = date, y = x)) +
  geom_line() +
  facet_wrap(~topic, scales = 'free') +
  theme_minimal()


t <- names(topic)[2:length(topic)]
out <- NULL
for(i in seq_along(t)){
  set.seed(2024)
  out[[i]] <- inner_join(
    topic %>% 
      select(date, sym(t[i])),
    fred %>% 
      select(date, R, M, HL, BL, TED, YC, BS, P, IP)
  ) %>% 
    select(-date) %>% 
    ts() %>% 
    vars::VAR(p = 12) %>%
    vars::SVAR(Amat=matrix(nrow = 10, 
                           ncol = 10,
                           c(1, NA, NA, NA ,NA, NA, NA ,NA, NA, NA,
                             0, 1, NA, NA, NA, NA, NA, NA, NA, NA,
                             0, 0, 1, NA, NA, NA, NA, NA, NA, NA,
                             0, 0, 0, 1, NA, NA, NA, NA, NA, NA,
                             0, 0, 0, 0, 1, NA, NA, NA, NA, NA,
                             0, 0, 0, 0, 0, 1, NA, NA, NA, NA, 
                             0, 0, 0, 0, 0, 0, 1, NA, NA, NA,
                             0, 0, 0, 0, 0, 0, 0, 1, NA, NA, 
                             0, 0, 0, 0, 0, 0, 0, 0, 1, NA,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 1))) %>% 
    vars::irf(impulse = t[i],
              response = c(t[i], 'R','M', 'HL', 'BL', 'TED', 'YC', 'BS', 'P', 'IP'),
              n.ahead = 60,
              ci = 0.84) 
}
names(out) <- t
saveRDS(out, 'data/svars2.rds')



[1] "banks"   "commod"  "confid"  "exch"    "ffr"     "finmark" "finstab" "fiscal"  "gdp"     "housing" "inflat" 
[12] "labmark" "monmark" "monpol"  "oper"    "recess"  "supp"    "trade"   "yield"   "PC1" 

out[[7]] %>% 
  plot()

out[[1]][[1]][[1]] %>% 
  as_tibble() %>% 
  mutate(t = row_number()) %>% 
  gather(key = var,
         value = value,
         -t) %>% 
  ggplot(aes(x = t, y = value)) +
  geom_line() +
  facet_wrap(~var)
