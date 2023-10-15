#webscraper - legfrissebb évek

scrape_latest <- function(url = "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)
  require(stringr)
  
  #read link  
  htmlread <- read_html(url)
  
  fomc <- NULL
  
  
  #get dates
  yearDoms <- htmlread %>% html_nodes("div.panel.panel-default")
  
  row <- 0
  for(yearI in 1:length(yearDoms)) {
    
    yearText <- yearDoms[[yearI]] %>% 
      html_nodes("div.panel-heading > h4 > a") %>% 
      html_text(TRUE) %>% substr(1,4)
    
    monthDoms <- yearDoms[[yearI]] %>% 
      html_nodes("div.row.fomc-meeting")
    
    for(monthI in 1:length(monthDoms)) {
      row <- row + 1
      
      monthText <- monthDoms[[monthI]] %>% 
        html_node("div.fomc-meeting__month.col-xs-5.col-sm-3.col-md-2 strong") %>% 
        html_text(TRUE)
      
      dateText <- monthDoms[[monthI]] %>% 
        html_node(".fomc-meeting__date.col-xs-4.col-sm-9.col-md-10.col-lg-1") %>% 
        html_text(TRUE)
      
      subUrl <- html_attr(monthDoms[[monthI]] %>% 
                            html_node(".col-xs-12.col-md-4.col-lg-4.fomc-meeting__minutes a:nth-of-type(2)") , "href")
      
      if(is.na(subUrl)) {
        content <- NA
      } else {
        content <- paste("https://www.federalreserve.gov", subUrl, sep = "") %>% 
          read_html() %>% 
          html_nodes("#article p") %>% 
          html_text(TRUE) %>% 
          as.character() %>% as_tibble() %>% list()
      }
      
      fomc[[row]] <- c(yearText, monthText, dateText) %>% t() %>% 
        as_tibble() %>% rename(year = V1,
                               month = V2,
                               day = V3) %>% 
        group_by(year, month, day) %>% nest()
      fomc[[row]] <- fomc[[row]] %>% mutate(data = content)
      
    }
    
  }
  
  fomc_df <- fomc %>% bind_rows()
  
  fomc_df
  
}


#webscraper - historical


scrape_historical <- function(year){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)
  require(stringr)
  
  #read link
  url <- paste("https://www.federalreserve.gov/monetarypolicy/fomchistorical", year,".htm", sep = "")
  htmlread <- read_html(url)
  
  scrape_info_html <- tibble(year_select = 1993:2016) %>% 
    mutate(main_handle = ifelse(year_select %in% c(2011:2016), 
                                "div.panel.panel-default.panel-padded",
                                "div#article"),
           panel_heading = ifelse(year_select %in% c(2011:2016),
                                  "h5.panel-heading",
                                  "div.panel-heading"),
           article_handle = ifelse(year_select %in% c(2012:2016),
                                   "#article p", NA),
           article_handle = ifelse(year_select %in% c(1993:2011), 
                                   "p", 
                                   article_handle))
  
  scrape_info_html <- scrape_info_html %>% filter(year_select == year)
  
  main_handle <- scrape_info_html %>% 
    pull(main_handle) 
  
  panel_heading <- scrape_info_html %>% 
    pull(panel_heading) 
  
  article_handle <- scrape_info_html %>% 
    pull(article_handle) 
  
  
  panels <- htmlread %>% 
    html_nodes(main_handle) 
  
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
    html_nodes(panel_heading) %>% html_text(TRUE) %>% 
    as_tibble() %>% 
    filter(grepl("Meeting", value)) %>% 
    rename(date_info = value) %>% 
    separate(date_info, sep = "Meeting", into = c("p1", "p2")) %>% 
    mutate(p2 = str_remove_all(p2, " - ")) %>% rename(year = p2) %>% 
    mutate(date = num_extract(p1),
           p1 = str_remove_all(p1, date),
           p1 = str_split(p1, "\\ ", simplify=T)[,1]) %>% rename(month = p1) %>% 
    select(year, month, date) %>% 
    distinct(month, .keep_all = TRUE)
  
  
  suburls <- panels %>% html_nodes("div.col-xs-12.col-md-6") %>%
    html_nodes("a") %>% html_attr("href") %>% as_tibble() %>% 
    filter(grepl("fomc", value) & !grepl("memos", value) & !grepl("beige", value),
           grepl("htm", value),
           grepl("htm$", value),
           !grepl("DEFAULT", value), !grepl("default", value),
           !grepl("presconf", value)) %>% 
    rename(suburl = value) %>% 
    mutate(suburl = paste("https://www.federalreserve.gov", suburl, sep = "")) %>% 
    as.matrix()
  
  
  content <- suburls %>% as_tibble() %>% 
    rename(data = suburl)
  
  for(i in 1:length(suburls)){
    content$data[i] <- suburls[i] %>% read_html() %>% 
      html_nodes(article_handle) %>% 
      html_text(TRUE) %>% 
      as.character() %>% 
      as_tibble() %>% rename(content = value) %>% list()
  }
  
  
  
  fomc <- bind_cols(months, content)
  
  fomc
}


scrape_historical_pdf <- function(year){
  
  require(rvest)
  require(dplyr)
  require(tidyr)
  require(xml2)
  require(stringr)
  require(pdftools)
  
  #read link
  url <- paste("https://www.federalreserve.gov/monetarypolicy/fomchistorical", year,".htm", sep = "")
  htmlread <- read_html(url)
  
  scrape_info_html <- tibble(year_select = 1936:2016) %>% 
    mutate(main_handle = ifelse(year_select %in% c(2011:2016), 
                                "div.panel.panel-default.panel-padded",
                                "div#article"),
           panel_heading = ifelse(year_select %in% c(2011:2016),
                                  "h5.panel-heading",
                                  "div.panel-heading"),
           suburl_handle = ifelse(year_select %in% c(1993:2016),"div.col-xs-12.col-md-6",
                                  "p"))
  
  scrape_info_html <- scrape_info_html %>% filter(year_select == year)
  
  main_handle <- scrape_info_html %>% 
    pull(main_handle) 
  
  panel_heading <- scrape_info_html %>% 
    pull(panel_heading) 
  
  suburl_handle <- scrape_info_html %>% 
    pull(suburl_handle)
  
  
  panels <- htmlread %>% 
    html_nodes(main_handle) 
  
  htmlread %>% html_nodes("div.panel.panel-default") %>% 
    html_nodes("div#article.col-xs-12.col-sm-8.col-md-9")
  
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
    html_nodes(panel_heading) %>% html_text(TRUE) %>% 
    as_tibble() %>% 
    filter(grepl("Meeting", value)) %>% 
    rename(date_info = value) %>% 
    separate(date_info, sep = "Meeting", into = c("p1", "p2")) %>% 
    mutate(p2 = str_remove_all(p2, " - ")) %>% rename(year = p2) %>% 
    mutate(date = num_extract(p1),
           p1 = str_remove_all(p1, date),
           p1 = str_split(p1, "\\ ", simplify=T)[,1]) %>% rename(month = p1) %>% 
    select(year, month, date) %>% 
    distinct(month, date, .keep_all = TRUE)
  
  
  suburls <- panels %>% html_nodes(suburl_handle) %>%
    html_nodes("a") %>% html_attr("href") %>% as_tibble() %>% 
    filter((grepl("fomc", value) | grepl("FOMC", value)) & !grepl("memos", value) & !grepl("beige", value),
           grepl("pdf", value),
           grepl("pdf$", value),
           !grepl("DEFAULT", value), !grepl("default", value),
           !grepl("presconf", value),
           !grepl("ropa", value),
           !grepl("hminec", value)) %>% 
    rename(suburl = value) %>% 
    mutate(suburl = paste("https://www.federalreserve.gov", suburl, sep = "")) %>% 
    as.matrix() 
  
  
  content <- suburls %>% as_tibble() %>% 
    rename(data = suburl)
  
  for(i in 1:length(suburls)){
    content$data[i] <- suburls[i] %>% pdf_text() %>% 
      as.character() %>% 
      as_tibble() %>% rename(content = value) %>% list()
  }
  
  
  
  fomc <- bind_cols(months, content)
  
  fomc
}
