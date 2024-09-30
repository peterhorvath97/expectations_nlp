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


inflexp <- fred_query(ids = c('EXPINF1YR', 'EXPINF3YR', 'EXPINF5YR', 'EXPINF10YR'),
                      freq = 'm',
                      start = '1900-01-01')

inflexp <- inflexp %>% 
  rename(PE1 = EXPINF1YR,
         PE3 = EXPINF3YR,
         PE5 = EXPINF5YR,
         PE10 = EXPINF10YR)

read_rds('data/uncertainty.rds') %>% 
  mutate(year = year(date),
         month = month(date),
         month = ifelse(nchar(month) == 1, paste('0', month, sep = ''), as.character(month)),
         date = paste(year, month, '01', sep = '-') %>% as_date()) %>% 
  select(-year, -month) %>% 
  group_by_at(vars(-dist)) %>% 
  mutate(dist = mean(dist))

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
saveRDS(out, 'data/svars.rds')


out2 <- NULL
for(i in seq_along(t)){
  set.seed(2024)
  out2[[i]] <- inner_join(
    topic %>% 
      select(date, sym(t[i])),
    fred %>% 
      select(date, R, P, IP)
  ) %>% 
    inner_join(inflexp %>% 
                 select(date, PE1)) %>% 
    select(-date) %>% 
    select(sym(t[i]), R, PE1, P, IP) %>% 
    ts() %>% 
    vars::VAR(p = 12) %>% 
    vars::SVAR(Amat = matrix(nrow = 5,
                             ncol = 5, 
                             c(1, NA, NA, NA, NA,
                               0, 1, NA, NA, NA,
                               0, 0, 1, NA, NA,
                               0, 0, 0, 1, NA,
                               0, 0, 0, 0, 1))) %>% 
    vars::irf(impulse = t[i],
              response = c(t[i], 'R', 'PE1', 'P', 'IP'),
              n.ahead = 60,
              ci = 0.84)
}
names(out2) <- t
saveRDS(out2, 'data/svars_small.rds')

out

[1] "banks"   "commod"  "confid"  "exch"    "ffr"     "finmark" "finstab" "fiscal"  "gdp"     "housing" "inflat" 
[12] "labmark" "monmark" "monpol"  "oper"    "recess"  "supp"    "trade"   "yield"   "PC1" 

out2[['fiscal']] %>% 
  plot()

out[['PC1']][[1]][[1]] %>% 
  as_tibble() %>% 
  mutate(t = row_number()) %>% 
  gather(key = var,
         value = value,
         -t) %>% 
  ggplot(aes(x = t, y = value)) +
  geom_line() +
  facet_wrap(~var)
