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

shocks <- topic

out <- NULL
for(i in seq_along(names(shocks)[2:length(names(shocks))])){
  instr <- names(shocks)[2:length(names(shocks))][i] %>% sym()
  
  data <- shocks %>% 
    select(date, instr) %>% 
    inner_join(fred) 
  
  ydata <- data %>% 
    select(date, R, M, YC, BS, TED, BL, HL, CP, P, IP, instr) %>% 
    select(-CP) %>% 
    select(-date, -instr) %>% 
    as.matrix()
  
  z <- data %>% 
    select(instr) %>%
    pull(instr) %>% 
    as.numeric()
  
  VAR <- varexternal::SVARIV(ydata = ydata,
                             z = z ,
                             p = 12, 
                             NWlags = 0,
                             norm = 1, 
                             scale = 1,
                             horizons = 60,
                             confidence = c(0.84,0.95),
                             instrument_name = 'instrument')
  
  out[[i]] <- VAR
  
}

names(out) <- names(shocks)[2:length(names(shocks))]
saveRDS(out, 'data/svarivs3.rds')


[1] "banks"   "busi"    "commod"  "confid"  "exch"    "ffr"     "finmark" "finstab" "fiscal"  "gdp"     "housing"
[12] "inflat"  "labmark" "monmark" "monpol"  "oper"    "recess"  "trade"   "yield"  


out[['total']]$irfs %>% 
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


out[['total']]$waldstat %>% 
  filter(type == 'Waldstat',
         level ==0.95)

rm(data, fred, inflexp, out, out2, shocks, VAR, ydata, 
   i, instr, z, fred_query)