print('Processing scraped JSON files.')
data1 <- fromJSON(file = file.path('data', 'final-recent.json'))[[1]] 
data2 <- fromJSON(file = file.path('data', 'final-hist.json'))[[1]]
data <- c(data1, data2)
rm(data1, data2)

n <- names(data[[1]])
for(i in 2:length(data)){
  n <- c(n,names(data[[i]]))
}
n <- unique(n)

for(m in 1:length(n)){
  for(i in 1:length(data)){
    if(names(data[[i]]) %>% 
      str_detect(n[m]) %>% 
      max() == 0) {
      data[[i]][n[m]] <- NA 
    }
  }
}

data <- lapply(data, as_tibble)
data <- lapply(data, function(x) mutate(x, size = as.character(size)))


data <- data %>% 
  bind_rows()

data <- data %>% 
  select(-mtg,
         -rldt) %>% 
  mutate(d = as_date(d),
         dt = as_date(dt),
         dt = ifelse(type == 'SEP', d, dt)) %>% 
  rename(mtg_date = d,
         pub_date = dt) %>% 
  mutate(pub_date = ifelse(type == "Mn" & is.na(pub_date), mtg_date + 20, pub_date) %>% as_date(),
         name = case_when(type == 'Mn' ~ 'Minutes',
                          type == 'St' ~ 'Statement',
                          type == 'Ag' ~ 'Agenda',
                          type == 'Prc' ~ 'Press Conference',
                          type == 'Trns' ~ 'Transcript',
                          type == 'Tl' ~ 'Tealbook',
                          type == 'SEP' ~ 'Projection',
                          type == 'Pj' ~ 'Projection',
                          type == 'PrMat' ~ 'Projection',
                          type == 'Bgbk' ~ 'Beigebook',
                          type == 'Rdbk' ~ 'Redbook',
                          type == 'Grbk' ~ 'Greenbook',
                          type == 'Blbk' ~ 'Bluebook',
                          type == 'Pl' ~ 'Note',
                          type == 'HMin' ~ 'Minutes',
                          type == 'MoA' ~ 'Minutes',
                          type == 'ROPA' ~ 'Ropa',
                          type == 'ExCommMin' ~ 'Minutes',
                          type == 'GrBl' ~ 'book',
                          type == 'MemD' ~ 'Memo',
                          TRUE ~ name)) %>% 
  mutate(name = ifelse(str_detect(name, 'book'), 'ColorBook', name)) %>% 
  select(-type, -size) 


data <- data %>% 
  mutate(files = map(files, ~as_tibble(.x)),
         f = map(files, ~length(.x)) %>% unlist(),
         files = ifelse(f == 0, map(url, ~as_tibble(.x)), files),
         files = map(files, ~.x %>% 
                       gather(key = key, value = value))
         ) %>% 
  unnest(files) %>% 
  filter(str_detect(value, or(DOT %R%'htm', DOT %R% 'pdf')) ) %>% 
  select(-url, -key, -f) %>% 
  rename(url = value) %>% 
  filter(!is.na(url)) %>% 
  distinct(url, .keep_all = T) %>% 
  mutate(ftype = ifelse(str_detect(url, 'pdf' %R% END), 'PDF', 'HTML'),
         url = ifelse(!str_detect(url, or('https://www.federalreserve.gov', 'http://www.federalreserve.gov')), 
                      paste('https://www.federalreserve.gov', url, sep = ''),
                      url),
         pub_date = ifelse(is.na(pub_date), mtg_date, pub_date) %>% as_date()
         ) 



data <- data %>% 
  mutate(id = url,
         id = str_replace_all(id, '/\\d\\d\\d\\d/', '/'),
         loc = str_locate_all(id, '/'),
         loc = map(loc, ~.x %>% max()) %>% unlist(),
         id = substr(url, loc, nchar(url)),
         id = str_remove_all(id, DOT %R% or('htm', 'pdf')),
         id = str_remove_all(id, or('a' %R% END, 'a' %R% DGT %R% END)),
         id = str_remove_all(id, '/')) %>% 
  select(-loc) %>% 
  group_by(id) %>% 
  select(-id) %>% 
  group_by(id, name) %>% 
  mutate(n = n()) %>%
  filter(n != 2 | (n == 2 & ftype == 'HTML')) %>%
  ungroup() %>% 
  select(-n,-id)
  
   
data <- data %>% 
  filter(!(name %in% c('Agenda', 'Note', 'Projection', 'Press Conference')),
         #!str_detect(url, 'SEPkey'),
         #!str_detect(url, 'projtab')
         !str_detect(url, 'confcall')
         ) 


saveRDS(data, file.path('data', 'fomclinks.rds'))

data %>% 
  mutate(year = year(mtg_date)) %>% 
  group_by(year, name) %>% 
  count() %>%  
  ggplot(aes(x = year, y = n, fill = name)) + 
  geom_col(color = 'black')
