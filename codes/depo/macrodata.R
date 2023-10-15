library(fredr)
library(dplyr)
library(tidyr)

fredr_set_key("cda47ae66b38ed7988c0a9c2ec80c94f")

cpi <- fredr("USACPIALLMINMEI",
             units = "ch1") %>% 
  select(date, value) %>% 
  drop_na()

infl_exp <- fredr("EXPINF1YR") %>% 
  select(date, value) %>% 
  drop_na()


#Infl?ci?s v?rakoz?s havi adat hosszabb id?sor
url <- "https://www.clevelandfed.org/our-research/indicators-and-data/~/media/content/our%20research/indicators%20and%20data/inflation%20expectations/ie%20latest/ie%20xls.xls?la=en"
library(readxl)
library(httr)
infl_exp_havi <- GET(url, write_disk(tf <- tempfile(fileext = ".xls")))[["content"]] %>% 
  read_excel(sheet = "Expected Inflation")
colnames(infl_exp_havi) <- c("date", paste("exp", 1:30, sep = "_"))

library(ggplot2)

inflexpdata <- GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
inflexpdata@content

infl_exp_havi %>% 
  gather(key = "indicator", value = "value", -date) %>% 
  ggplot(aes(x = date, y = value, group = indicator, color = indicator)) +
  geom_line()

infl_exp_havi %>% 
  gather(key = "indicator", value = "value", -date) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~indicator)
