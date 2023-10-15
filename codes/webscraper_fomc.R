library(rvest)
library(dplyr)
library(tidyr)
library(xml2)
library(stringr)
library(pdftools)
library(rebus)

#Functions for scraping FOMC text data
scrape_pdfs <- function(url){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)

   
  
df <- url %>% read_html() %>% html_nodes("div#article.col-xs-12.col-sm-8.col-md-9") %>% 
  html_nodes("a") %>% html_attr("href") %>% 
  as_tibble() %>% filter(str_detect(value, "pdf")) 

df
}

scrape_htmls <- function(url){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)


df <- url %>% read_html() %>% html_nodes("div#article.col-xs-12.col-sm-8.col-md-9") %>% 
  html_nodes("a") %>% html_attr("href") %>% 
  as_tibble() %>% filter(!str_detect(value, "pdf"))  

df
}

gen_urls <- function(years_historical){
urls <- NULL
for(i in 1:length(years_historical)){
  urls[i] <- paste("https://www.federalreserve.gov/monetarypolicy/fomchistorical", 
                   years_historical[i], ".htm", sep = "")
}
urls <- c(urls, "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm")
urls
}

scrape_and_clean <- function(urls){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)
  require(stringr)
  require(rebus)
  
fomc_list_html <- NULL
fomc_list_pdf <- NULL

for(i in 1:length(urls)){
  Sys.sleep(runif(1,2,6))
  fomc_list_html[[i]] <- scrape_htmls(urls[i])
  fomc_list_pdf[[i]] <- scrape_pdfs(urls[i])
  closeAllConnections()
    
}

fomc_html <- bind_rows(fomc_list_html) %>% 
  mutate(filetype = "html")
fomc_pdf <- bind_rows(fomc_list_pdf) %>% 
  mutate(filetype = "pdf")

digit_8 <- DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(one_or_more(DGT))

fomc_data <- bind_rows(fomc_html, fomc_pdf) %>% 
  #create date index variables
  mutate(
    #crerate 8+ digit numeric value from scraped links
    num = str_extract(value, digit_8),
    num = ifelse(nchar(num) == 8, num, substr(num, 2,9)),
    year = substr(num, 1,4),
    month = substr(num, 5,6),
    day = substr(num, 7,8)) %>% 
  #keep only data where the date is recognizable from the link
  filter(!is.na(num)) %>% 
  #extract and clean document type
  mutate(
    doctype = str_remove_all(value, num) %>% 
      str_to_lower() %>% str_remove_all(".htm") %>% str_remove_all(".pdf") %>% 
      str_replace_all(exactly("/monetarypolicy/fomc"), "minutes"),
    doctype = ifelse(str_detect(doctype, "beige"), "beigebook", doctype),
    doctype = ifelse(str_detect(doctype, "green") | str_detect(doctype, "gb"), "greenbook", doctype),
    doctype = ifelse(str_detect(doctype, "blue"), "bluebook", doctype),
    doctype = ifelse(str_detect(doctype, "agenda"), "agenda", doctype),
    doctype = ifelse(str_detect(doctype, "conf"), "confcall", doctype),
    doctype = ifelse(str_detect(doctype, "mod"), "memorandum_disc", doctype),
    doctype = ifelse(str_detect(doctype, "teal"), "tealbook", doctype),
    doctype = ifelse(str_detect(doctype, "red"), "redbook", doctype),
    doctype = ifelse(str_detect(doctype, "fomcproj"), "projection", doctype),
    doctype = ifelse(str_detect(doctype, "meeting"), "transcript", doctype),
    doctype = ifelse(str_detect(doctype, "minutes") | 
                       str_detect(doctype, "moa") |
                       str_detect(doctype, "min") & !str_detect(doctype, "minec"), "minutes", doctype),
    doctype = ifelse(str_detect(doctype, "default") |
                       str_detect(doctype, "press") & !str_detect(doctype, "monetary" %R% 
                                                                    or("b", "c") %R% optional(DGT)) &
                       !str_detect(doctype, "a1") |
                       str_detect(doctype, "monetarya" %R% END) |
                       str_detect(doctype, "ropa"), "statement", doctype),
    doctype = ifelse(str_detect(doctype, "/"), "other material", doctype)) %>% 
  distinct(value, .keep_all = TRUE) %>% 
  filter(value != "https://www.federalreserve.gov/monetarypolicy/files/FOMChminfc219370126.pdf",
         !str_detect(value, "zip$")) %>% 
  #some extra prep
  mutate(value = ifelse(!str_detect(value, "^http"), 
                        paste("https://www.federalreserve.gov", value, sep = ""), 
                        value)) %>% 
  select(year, month, day, filetype, doctype, link = value)


fomc_data
}

get_statements <- function(fomc_data){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)
  require(pdftools)
  
  filtering_statements <- fomc_data %>% 
    filter(doctype == "statement") %>% 
    group_by(filetype, year, month, day) %>% count() %>% 
    spread(filetype, n) %>% 
    mutate(filtering_pdf = ifelse(!is.na(pdf) & is.na(html), T, F),
           filtering_html = ifelse(!is.na(html), T, F)) %>% 
    select(year, month, day, filtering_pdf, filtering_html)
  
  statements_pdf <- fomc_data %>% 
    filter(doctype == "statement",
           filetype == "pdf") %>% 
    left_join(filtering_statements, by = c("year", "month", "day")) %>% 
    filter(filtering_pdf == TRUE) %>% 
    select(year, month, day, filetype, doctype, link)
  
  statements_html <- fomc_data %>% 
    filter(doctype == "statement",
           filetype == "html") %>% 
    left_join(filtering_statements, by = c("year", "month", "day"))  %>% 
    filter(filtering_html == TRUE) %>% 
    select(year, month, day, filetype, doctype, link)
  
  statements_pdf <- statements_pdf %>% mutate(data = NA)
  for(i in 1:nrow(statements_pdf)){
    Sys.sleep(runif(1,2,6))
    statements_pdf$data[i] <- statements_pdf$link[i] %>% 
      pdf_text() %>% as_tibble() %>% list()
    closeAllConnections()
  }
  
  statements_html <- statements_html %>% mutate(data = NA)
  for(i in 1:nrow(statements_html)){
    Sys.sleep(runif(1,2,6))
    statements_html$data[i] <-statements_html$link[i] %>% read_html() %>% 
      html_nodes("p") %>% 
      html_text(TRUE) %>% 
      as_tibble() %>% list()
    closeAllConnections()
  }
  
  statements_all <- bind_rows(statements_pdf, statements_html) %>% 
    arrange(year, month, day)
  
  statements_all
}

get_minutes <- function(fomc_data){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)
  require(pdftools)
  
  
  filtering_minutes <- fomc_data %>% 
    filter(doctype == "minutes") %>% 
    group_by(filetype, year, month, day) %>% count() %>% 
    spread(filetype, n) %>% 
    mutate(filtering_pdf = ifelse(!is.na(pdf) & is.na(html), T, F),
           filtering_html = ifelse(!is.na(html), T, F)) %>% 
    select(year, month, day, filtering_pdf, filtering_html)
  
  minutes_pdf <- fomc_data %>% 
    filter(doctype == "minutes",
           filetype == "pdf") %>% 
    left_join(filtering_minutes, by = c("year", "month", "day"))  %>% 
    filter(filtering_pdf == TRUE) %>% 
    select(year, month, day, filetype, doctype, link)
  
  minutes_html <- fomc_data %>% 
    filter(doctype == "minutes",
           filetype == "html") %>% 
    left_join(filtering_minutes, by = c("year", "month", "day"))  %>% 
    filter(filtering_html == TRUE) %>% 
    select(year, month, day, filetype, doctype, link)
  
  
  minutes_pdf <- minutes_pdf %>% mutate(data = NA)
  for(i in 1:nrow(minutes_pdf)){
    Sys.sleep(runif(1,2,6))
    minutes_pdf$data[i] <- minutes_pdf$link[i] %>% 
      pdf_text() %>% as_tibble() %>% list()
    closeAllConnections()
    
  }
  
  minutes_html <- minutes_html %>% mutate(data = NA)
  for(i in 1:nrow(minutes_html)){
    Sys.sleep(runif(1,2,6))
    minutes_html$data[i] <-minutes_html$link[i] %>% read_html() %>% 
      html_nodes("p") %>% 
      html_text(TRUE) %>% 
      as_tibble() %>% list()
    closeAllConnections()
  }

  
  minutes_all <- bind_rows(minutes_pdf, minutes_html) %>% 
    arrange(year, month, day)
  
  minutes_all  
}



#Execute scraping
urls <- gen_urls(1936:2016)

fomc_data <- scrape_and_clean(urls)

statements <- get_statements(fomc_data)

minutes <- get_minutes(fomc_data)


library(readr)

folder <- "saved_data"
write_rds(fomc_data, file.path(folder, "fomc_data.rds"))
write_rds(minutes, file.path(folder, "minutes.rds"))
write_rds(statements, file.path(folder, "statements.rds"))



#Issues:
#Record of policy actions = Statement?
#HTM - PDF issues
#Date matching
#Date allocation
#Potential mixed frequency?




