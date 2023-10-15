scrape_latest_old <- function(url = "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"){
  
  require(rvest)
  require(dplyr)
  require(xml2)
  
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
          as.character() %>% paste(sep = " ", collapse = " ")
      }
      
      fomc[[row]] <- c(yearText, monthText, dateText, content) %>% t() %>% 
        as_tibble() %>% rename(year = V1,
                               month = V2,
                               day = V3,
                               content = V4)
      
    }
    
  }
  
  fomc_df <- fomc %>% bind_rows() %>% 
    group_by(year, month, day) %>% nest()
  
  fomc_df
  
}
