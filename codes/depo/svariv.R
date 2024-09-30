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



shocks <- read_rds('data/shocks.rds')

shocks <- shocks %>% 
  drop_na() %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(year, month) %>% 
  summarize(across(c(-date), ~mean(.x, na.rm = T))) %>% 
  mutate(month = as.character(month),
         month = ifelse(nchar(month) >1, month, paste('0', month, sep = ''))) %>% 
  mutate(date = paste(year, month, '01', sep = '-')) %>% 
  ungroup() %>% 
  select(-year, -month) %>% 
  relocate(date, .before = banks) %>% 
  mutate(date = as_date(date))


out2 <- NULL
for(i in seq_along(names(shocks)[2:length(names(shocks))])){
  instr <- names(shocks)[2:length(names(shocks))][i] %>% sym()
  
  data <- shocks %>% 
    select(date, instr) %>% 
    inner_join(fred) %>% 
    inner_join(inflexp)
  
  ydata <- data %>% 
    select(date, R, PE1, P, IP, instr) %>% 
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
  
  out2[[i]] <- VAR
  
}
names(out2) <- names(shocks)[2:length(names(shocks))]
saveRDS(out2, 'data/svarivs_small.rds')



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
saveRDS(out, 'data/svarivs.rds')


[1] "banks"   "busi"    "commod"  "confid"  "exch"    "ffr"     "finmark" "finstab" "fiscal"  "gdp"     "housing"
[12] "inflat"  "labmark" "monmark" "monpol"  "oper"    "recess"  "trade"   "yield"   "total"   "PC1" 

out <- read_rds('data/svarivs.rds')
out2 <- read_rds('data/svarivs_small.rds')

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