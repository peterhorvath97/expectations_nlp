print('Scraping textual data from individual document links.')
data <- read_rds(file.path('data', 'fomclinks.rds')) %>% 
  mutate(text = NA)

data_html <- data %>% filter(ftype == 'HTML')
data_pdf <- data %>% filter(ftype == 'PDF')


print('Scraping HTML-s.')
n <- nrow(data_html)
for(i in 1:n){
  
  cat(paste0(rep('=', i / n * options()$width), collapse = ''))
  
  Sys.sleep(runif(1,2,6))
  
  data_html$text[i] <- data_html$url[i] %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text(TRUE) %>% 
    as_tibble() %>% 
    list()
  closeAllConnections()
  
  if (i == n) cat('\014Done')
  else cat('\014')
  
}

print('Scraping PDF-s.')
n <- nrow(data_pdf)
for(i in 1:n){
  
  cat(paste0(rep('=', i / n * options()$width), collapse = ''))
  
  Sys.sleep(runif(1,2,6))
  
  data_pdf$text[i] <- data_pdf$url[i] %>% 
    pdf_text() %>%  
    as_tibble() %>% 
    list()
  closeAllConnections()
  
  if (i == n) cat('\014Done')
  else cat('\014')
  
}

data <- bind_rows(data_html, data_pdf)

saveRDS(data, file.path('data', 'fomc_text_raw.rds'))
