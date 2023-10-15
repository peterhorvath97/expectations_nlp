scrape_historical_2011_2016 <- function(year){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)
  require(stringr)
  
  #read link
  year <- year
  url <- paste("https://www.federalreserve.gov/monetarypolicy/fomchistorical", year,".htm", sep = "")
  htmlread <- read_html(url)
  
  
  panels <- htmlread %>% 
    html_nodes("div.panel.panel-default.panel-padded") 
  
  num_extract <- function(str){
    numlist <- regmatches(str, gregexpr("[[:digit:]]+", str))
    
    for(i in 1: length(numlist)){
      if (length(numlist[[i]]) == 2)
        numlist[[i]] <- paste(numlist[[i]][1], numlist[[i]][2], sep = "-")
      else
        numlist[[i]] <- numlist[[i]]
    }
    unlist(numlist)
  }
  
  months <- panels %>%  
    html_nodes("h5.panel-heading") %>% html_text(TRUE) %>% 
    as_tibble() %>% 
    filter(grepl("Meeting", value)) %>% 
    rename(date_info = value) %>% 
    separate(date_info, sep = "Meeting", into = c("p1", "p2")) %>% 
    mutate(p2 = str_remove_all(p2, " - ")) %>% rename(year = p2) %>% 
    mutate(date = num_extract(p1),
           p1 = str_remove_all(p1, date),
           p1 = str_split(p1, "\\ ", simplify=T)[,1]) %>% rename(month = p1) %>% 
    select(year, month, date)
  
  
  suburls <- panels %>% html_nodes("div.col-xs-12.col-md-6") %>%
    html_nodes("a") %>% html_attr("href") %>% as_tibble() %>% 
    filter(grepl("fomcminutes", value),
           grepl("htm", value)) %>% 
    rename(suburl = value) %>% 
    mutate(suburl = paste("https://www.federalreserve.gov", suburl, sep = "")) %>% 
    as.matrix()
  
  
  content <- suburls %>% as_tibble() %>% 
    rename(data = suburl)
  
  for(i in 1:length(suburls)){
    content$data[i] <- suburls[1] %>% read_html() %>% 
      html_nodes("#article p") %>% 
      html_text(TRUE) %>% 
      as.character() %>% as_tibble() %>% list()
  }
  
  
  fomc <- bind_cols(months, content)
  
  fomc
}


scrape_historical_1993_2010 <- function(year){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)
  require(stringr)
  
  #read link
  year <- year
  url <- paste("https://www.federalreserve.gov/monetarypolicy/fomchistorical", year,".htm", sep = "")
  htmlread <- read_html(url)
  
  
  panels <- htmlread %>% 
    html_nodes("div#article") 
  
  num_extract <- function(str){
    numlist <- regmatches(str, gregexpr("[[:digit:]]+", str))
    
    for(i in 1: length(numlist)){
      if (length(numlist[[i]]) == 2)
        numlist[[i]] <- paste(numlist[[i]][1], numlist[[i]][2], sep = "-")
      else
        numlist[[i]] <- numlist[[i]]
    }
    unlist(numlist)
  }
  
  months <- panels %>%  
    html_nodes("div.panel-heading") %>% html_text(TRUE) %>% 
    as_tibble() %>% 
    filter(grepl("Meeting", value)) %>% 
    rename(date_info = value) %>% 
    separate(date_info, sep = "Meeting", into = c("p1", "p2")) %>% 
    mutate(p2 = str_remove_all(p2, " - ")) %>% rename(year = p2) %>% 
    mutate(date = num_extract(p1),
           p1 = str_remove_all(p1, date),
           p1 = str_split(p1, "\\ ", simplify=T)[,1]) %>% rename(month = p1) %>% 
    select(year, month, date)
  
  
  suburls <- panels %>% html_nodes("div.col-xs-12.col-md-6") %>%
    html_nodes("a") %>% html_attr("href") %>% as_tibble() %>% 
    filter(grepl("fomc", value) & !grepl("memos", value) & !grepl("beige", value),
           grepl("htm", value),
           grepl("htm$", value),
           !grepl("DEFAULT", value), !grepl("default", value)) %>% 
    rename(suburl = value) %>% 
    mutate(suburl = paste("https://www.federalreserve.gov", suburl, sep = "")) %>% 
    as.matrix()
  
  
  content <- suburls %>% as_tibble() %>% 
    rename(data = suburl)
  
  for(i in 1:length(suburls)){
    content$data[i] <- suburls[1] %>% read_html() %>% 
      html_nodes("#article p") %>% 
      html_text(TRUE) %>% 
      as.character() %>% as_tibble() %>% list()
  }
  
  
  fomc <- bind_cols(months, content)
  
  fomc
}
