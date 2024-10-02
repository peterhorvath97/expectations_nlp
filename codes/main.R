library(tidyverse)
library(magrittr)
library(stringr)
library(rebus)
library(rjson)
library(lubridate)
library(rvest)
library(xml2)
library(pdftools)
library(parallel)

library(tm)
library(tidytext)
library(qdap)
library(SnowballC)
library(textstem)

library(udpipe)

library(topicmodels)
library(topicdoc)
library(slam)
library(textdata)

library(fredr)
library(sandwich)
library(varexternal)



fomc_updates <- F
preprocess <- F
lang <- F




source('codes/after_pull.R')
unzip('data', 
      !is_empty(list.dirs(path = 'data', full.names = TRUE, recursive = FALSE)))
rm(unzip)

if(fomc_updates == T){
  source('codes/proc_json.R')
  source('codes/scraper.R')
}

if(preprocess == T){
  source('codes/preprocess.R')
  source('codes/tokenize.R')
  source('codes/cleanup.R')
}

if(lang == T){
  source('codes/vocab.R')
  source('codes/lda_fit.R')
  source('codes/lda_pred.R')
}




source('codes/before_push.R')
zip('data', 
    max(file.info(list.files('data', full.names = TRUE))$size>25*1024*1024)==1)
rm(zip)


