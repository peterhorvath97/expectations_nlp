
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
  'PCEPI','INDPRO', 'BUSLOANS', 'M1SL', 'FEDFUNDS',
  #Household Loans
  'CONSUMER', 'REALLN',
  #TED spread
  'MED3', 'TB3MS',
  #Bond spread
  'AAA', 'BAA',
  #Yield Curve
  'GS10'),
           freq = 'm',  
           start = '1950-01-01')


fred <- fred %>% 
  mutate(TED = MED3-TB3MS,
         HL = CONSUMER + REALLN,
         BS = BAA-AAA,
         YC = GS10-TB3MS) %>% 
  select(-MED3, -TB3MS,
         -CONSUMER, -REALLN,
         -BAA, -AAA,
         -GS10) %>% 
  rename(BL = BUSLOANS,
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

createlpspace <- function(data, var, lags, leads){
  data %>% 
    #Create RHS
    mutate(map_dfc(seq(lags), ~ lag(value, n = .x)) %>%
             set_names(paste('lag', seq(lags),sep = ''))) %>% 
    #Create LHS
    mutate(map_dfc(seq(leads), ~ lead(value, n = .x)) %>%
             set_names(paste('lead', seq(leads),sep = '')))
}


inflexp <- inflexp %>% 
  gather(key = var,
         value = value,
         -date) %>% 
  group_by(var) %>% 
  createlpspace(value, 12, 61) %>% 
  select(-value) %>% 
  nest()

fred <- fred %>% 
  gather(key = var,
         value = value,
         -date) %>% 
  group_by(var) %>% 
  createlpspace(value, 12, 61) %>% 
  select(-value) %>% 
  nest()

shocks <- shocks %>% 
  gather(key = var,
         value = value,
         -date) %>% 
  group_by(var) %>% 
  createlpspace(value, 12, 61) %>%
  select(-starts_with('lead')) %>% 
  rename(shock = value,
         shock1 = lag1,
         shock2 = lag2,
         shock3 = lag3,
         shock4 = lag4,
         shock5 = lag5,
         shock6 = lag6,
         shock7 = lag7,
         shock8 = lag8,
         shock9 = lag9,
         shock10 = lag10,
         shock11 = lag11,
         shock12 = lag12) %>% 
  nest()


vars <- bind_rows(fred, inflexp)
rm(fred, inflexp)

out2 <- NULL
for(j in 1:nrow(shocks)){

  shock <- shocks[j, ] %>% 
    unnest(data) 
  shockname <- shock %>% 
    distinct(var) %>% 
    pull(var)
  shock <- shock %>% 
    ungroup %>% 
    select(-var)
  
  out1 <- NULL
  for(i in 1:nrow(vars)){
    
    var <- vars[i, ] %>% 
      unnest(data) 
    varname <- var %>% 
      distinct(var) %>% 
      pull(var)
    var <- var %>% 
      ungroup %>% 
      select(-var)
    
  data <- full_join(shock, var)  
  
  x <- data %>% 
    select(starts_with('shock')|starts_with('lag')) %>% 
    as.matrix()
  y <- data %>% 
    select(starts_with('lead')) %>% 
    as.matrix()
  m <- round(0.75*sqrt(nrow(x)))
  h <- ncol(y)
  out <- NULL
  for(k in 1:h){
    model <- lm(y[,k] ~ -1 + x)
    irf <- model$coefficients[!str_detect(names(model$coefficients), DGT)]
    names(irf) <- shockname
    sum <- summary(model)
    nw <- NeweyWest(model, 
                    lag = m - 1, 
                    prewhite = F, 
                    adjust = T)
    se <- sqrt(diag(nw))[1:(ncol(x))]
    se <- se[!str_detect(names(se), DGT)]
    
    names(se) <- shockname
    irf_ub <- irf + se
    names(irf_ub) <- paste(names(irf_ub), '_ub', sep = '')
    irf_lb <- irf - se
    names(irf_lb) <- paste(names(irf_lb), '_lb', sep = '')
    
    out[[k]] <- as_tibble(t(c(irf, irf_ub, irf_lb))) %>% 
      gather(key = key,
             value = value) %>% 
      mutate(key2 = case_when(str_detect(key, 'ub') ~ 'ub',
                              str_detect(key, 'lb') ~ 'lb',
                              TRUE ~ 'mean'),
             key = str_remove_all(key, key2) %>% 
               str_remove_all('_' %R% END)) %>% 
      spread(key2, value) %>% 
      rename(shock = key) %>% 
      mutate(var = varname) %>% 
      mutate(t = k)
  }
  out <- bind_rows(out)
  out1[[i]] <- out 
  names(out1)[i] <- varname
  }
  out1 <- bind_rows(out1)
  out2[[j]] <- out1
  names(out2)[j] <- shockname
}
out2 <- bind_rows(out2)



out2 %>% 
  distinct(shock) %>% pull()

out2 %>% 
  filter(shock == 'monpol') %>% 
  ggplot(aes(x = t, y = mean, ymin = lb, ymax = ub)) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2) +
  geom_hline(yintercept = 0, color ='red', linetype = 'dashed')+
  facet_wrap(~var, scales = 'free') +
  theme_minimal()
