library(tidyverse)
library(dplyr)
library(tidyr)
library(tm)
library(stringr)
library(rebus)
library(readr)
library(purrr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(tidytext)
library(qdap)
library(SnowballC)
library(seededlda)

#First, read the data that was scraped from the FOMC meeting websites
minutes <- read_rds(file.path("saved_data", "minutes.rds")) %>%
  #Köszi az extramunkát FED :)
  group_by(year, month) %>% 
  mutate(dif_day = as.numeric(day) - as.numeric(lag(day)),
         filetype_match = lag(filetype)==filetype ) %>%
  filter(filetype_match != FALSE | is.na(filetype_match)) %>% 
  ungroup() %>% select(-dif_day, -filetype_match) 

statements <- read_rds(file.path("saved_data", "statements.rds")) 

#Get sats such as number of characters, documents, occurrences
get_text_stats <- function(df){
  
  require(tidyverse)
  
  df <- df %>% mutate(date = as.Date(paste(year, month, day, sep = "-")),
                      data = map(data,
                                 ~mutate(.x,
                                         value = str_conv(value, "UTF8"))),
                      data = ifelse(filetype == "pdf", 
                                    map(data,
                                        ~separate_rows(.x, value, sep = "\n\n")),
                                    data),
                      data = map(data,
                                 ~mutate(.x,
                                         nchar = nchar(value))),
#                      data = map(data,
#                                 ~filter(.x, nchar > 100)),
                      nchar_total = map(data, 
                                        ~summarize(.x,
                                                   sum(nchar))) %>% unlist(),
                      n_docs = map(data, ~summarize(.x, n = n())) %>% unlist()) %>% 
    select(date, filetype, data, nchar_total, n_docs) %>% 
    mutate(data = map(data,
                      ~select(.x, -nchar)))
  
  df
}
  
statements <-get_text_stats(statements) #%>% 
#  filter(date > as.Date("1993-01-01"))
minutes <- get_text_stats(minutes) #%>% 
#  filter(date > as.Date("1968-01-01"))

#Plot these out
text_param_viz <- function(minutes, statements){
  
  require(ggplot2)
  require(ggthemes)
  require(cowplot)
  require(dplyr)
  
char_statement <- statements %>% ggplot(aes(x = date, y = nchar_total)) +
  geom_point() +
  theme_stata() +
  ggtitle("Total character count per FOMC statement")

ndoc_statement <- statements %>% ggplot(aes(x = date, y = n_docs)) +
  geom_point() +
  theme_stata() +
  ggtitle("Number of documents per FOMC statement") +
  labs(caption = "document = html paragraph or pdf page")


char_minutes <- minutes %>% ggplot(aes(x = date, y = nchar_total)) +
  geom_point() +
  theme_stata() +
  ggtitle("Total character count per FOMC minutes")

  

ndoc_minutes <- minutes %>% ggplot(aes(x = date, y = n_docs)) +
  geom_point() +
  theme_stata() +
  ggtitle("Number of documents per FOMC minutes") +
  labs(caption = "document = html paragraph or pdf page")


n_statement <- statements %>% mutate(year = year(date)) %>% 
  group_by(year) %>% count() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col(color = "black", fill = "lightblue") +
  theme_stata() +
  ggtitle("Number of FOMC statements per annum")

n_minutes <- minutes %>% mutate(year = year(date)) %>% 
  group_by(year) %>% count() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col(color = "black", fill = "lightblue") +
  theme_stata() +
  ggtitle("Number of FOMC minutes per annum")


plot_grid(char_statement, char_minutes,
          ndoc_statement, ndoc_minutes,
          n_statement, n_minutes,
          nrow = 3,
          ncol = 2
)

}

text_param_viz(minutes #%>% filter(date > as.Date("1968-01-01"))
               , 
               statements #%>% filter(date > as.Date("1993-01-01"))
               )

#Clean the text data
clean_text <- function(df, nchars){
  
  require(tidyverse)
  require(tidytext)
  require(SnowballC)
  require(qdap)
  require(tm)
  
  custom_stops <- c("FOMC", "Federal Reserve System", "Federal Reserve", "Federal Open Market Comittee",
                    "statement", "Statement", "Board of Governors", "System", "Governor", "Federal", "federal",
                    "Governors", "Washington", "present", "Present", "Minutes", "d.c.",
                    "January", "February", "March",
                    "April", "May", "June", 
                    "July","August", "September",
                    "October", "November", "December",
                    "Mr", "Ms", "Mrs", "Meesrs", "Meessrs", "Meesr", "Miss", "Messrs",
                    "Board", "Governor", "Committee", "D. C.", "Federal",
                    "Office", "meeting", "Meeting",
                    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                    "Saturday", "Sunday", "staff", "Staff",
                    "Secretary", "Deputy", "Chairman", "Vice", "Assistant", "Director",
                    "Associate", "Division", "Senior", "Adviser", "Research and Statistics", "Chief",
                    "President", "Economist", "Economists", "Counselor", "General", "Manager",
                    "Managing",  "Open Market" ,"Counsel", "Minutes of Actions", "Chair", "internship", "Internship", "Internet", "internet",
                    "Minutes of Actions", "president", "President", "intern", "Intern", "Interns", "interns", "officer", "Officer",
                    "central bank", "United States", "New York", "Chicago", "Minneapolis", "San Francisco", "city", "City",
                    "Richmond", "Atlanta", "Dallas", "Cleveland", 
                    "City", "Kansas", "Philadelphia", "Boston", "Secretariat", "secretariat", 
                    "he", "He", "she", "She", "it", "It", "This", "this", "That", "that", "the", "The", "Respect", "respect",
                    "with", "With", "without", "Without", "made", "Made", "District", "district",
                    "per", "cent", "for", "For")
 
   custom_stops <- c("\r", "\n", "\t",
                    START %R% zero_or_more(SPACE) %R%  rebus::or("Mr", "Ms","Messrs", "Mrs", "Meesrs", "Meessrs", "Meesr", "Miss") %R% one_or_more(ANY_CHAR),
                    DOT %R% one_or_more(SPACE) %R% "[A-Z]" %R% one_or_more("[a-z]"), CURRENCY_SYMBOLS,
                    custom_stops, toupper(custom_stops))
  
df <- df %>% 
  mutate(data_clean = 
           map(data, ~mutate(.x,  value = value %>%
                                      removeWords(custom_stops) %>% 
                                      removeNumbers() %>% 
                                      stripWhitespace() %>% 
                                      bracketX() %>% 
                                      removeWords(stopwords("en")) %>% 
                                      removePunctuation() %>% 
                                      tolower()) %>%
                               filter(value != "",
                                      value != " ") %>% mutate(nchar = nchar(value)) %>% 
                               filter(nchar > nchars) %>%  select(-nchar) %>% 
                 mutate(doc = row_number(),
                        value = str_replace_all(value, "gross domestic product", "output") %>% 
                          str_replace_all("gross national product", "output") %>% 
                          str_replace_all("gdp", "output") %>% 
                          str_replace_all("gnp", "output")) %>% 
                 unnest_tokens(output = "word",
                               input = "value",
                               token = "words") %>% 
                 mutate(word = wordStem(word),
                        word = ifelse(word == "inflat", "inflation", word),
                        word = ifelse(word == "cpi", "inflation", word),
                        word = ifelse(word == "pce", "inflation", word),
                        word = ifelse(word == "price", "inflation", word),
                        word = ifelse(word == "increas", "increase", word),
                        word = ifelse(word == "decreas", "decrease", word),
                        word = ifelse(word == "inflationari", "inflation", word)) %>% 
                 filter(nchar(word) > 2) %>% 
                 group_by(doc) %>% 
                 summarize(value = str_c(word, collapse = " ")) %>% 
                 select(value)
                 ))
                               
}



minutes <- clean_text(minutes, nchars = 50)  %>% 
  mutate(doctype = "minutes")
statements <- clean_text(statements, nchars = 50) %>% 
  mutate(doctype = ifelse(year(date) >= 1993, "statements", "ropa"))

data <- bind_rows(minutes, statements) %>% 
  arrange(date)

data %>% 
  select(data_clean) %>% 
  unnest() %>% 
  mutate(doc = row_number()) %>% 
  unnest_tokens("word", "value", "words") %>% 
  count(doc, word) %>% 
  dfm()

#Create pooled document term matrix 
pool_dtm <- function(df, sparsity){
  
  require(tidyverse)
  require(tidytext)
  require(tm)
  
dtm_pooled <- df %>% 
  select(data_clean) %>% 
  unnest(cols = "data_clean") %>% 
  mutate(doc = row_number()) %>% 
  unnest_tokens(output = "word",
                input = "value",
                token = "words") %>% 
  count(doc, word) %>% 
  bind_tf_idf(term = word, document = doc, n = n) %>% 
  mutate(n = round(tf_idf*10*n)) %>% 
  cast_dtm(document = doc, term = word, value = n) %>% 
  removeSparseTerms(sparsity) %>% 
  tidy() %>% 
  filter(count != 0) %>% 
  cast_dtm(document, term, count) 

}

data_pooled <- pool_dtm(data, .9999) 

data_pooled %>% tidy() %>% quanteda::corpus(text_field = "term",
                                            docid_field = "document")

x <- data %>% select(data_clean) %>% 
  unnest(cols = "data_clean") %>% 
  mutate(doc = row_number()) %>% 
  corpus(text_field = "value",
         docid_field = "doc")

data %>% select(data_clean) %>% 
  unnest(cols = "data_clean") %>% 
  mutate(doc = row_number()) %>% 
  tokenize_word()
  
  textmodel_seededlda(dictionary = dictionary(list(increase = c("increase", "rise", "grow", "growth"),
                                                   decrease = c("decrease", "drop", "fall", "declin"))),
                      residual = FALSE, min_termfreq = 5,
                      verbose = TRUE)

#Cerate sentiment dictionary
lda_weights <- function(dtm_pooled) {

  require(seededlda)
  require(tidyverse)
  
mod <- textmodel_seededlda(dtm_pooled %>% tidy() %>% 
                             cast_dfm(document, term, count),
                           dictionary = dictionary(list(increase = c("increase", "rise", "grow", "growth"),
                                                        decrease = c("decrease", "drop", "fall", "declin"))),
                           residual = FALSE, min_termfreq = 5,
                           verbose = TRUE)

dict <- mod[["phi"]] %>% 
  t() %>% 
  as_tibble() %>% 
  mutate(term = colnames(mod[["phi"]])) %>% 
  select(term, increase, decrease) %>% 
  mutate(across(c(increase, decrease), ~exp(.x))) %>% 
  mutate(weight = increase - decrease,
         weight = (weight - min(weight))/(max(weight) - min(weight)),
         weight = (weight-0.5)/0.5) %>% 
  select(term, weight)

list(mod, dict)
}

set.seed(2022)
lda <- lda_weights(data_pooled)



#Find word associations
assocfinder <- function(df, word, corlimit){
  df %>% 
    unlist() %>% 
    DocumentTermMatrix %>% 
    removeSparseTerms(sparse = .9999) %>% 
    weightTfIdf %>% 
    findAssocs(word, corlimit = corlimit) %>% 
    data.frame %>% 
    rownames_to_column %>% 
    as_tibble %>% 
    rename(word = rowname,
           cor = !!word )
}

data <- data %>% 
  mutate(assocs = map(data_clean, ~assocfinder(.x, word = "inflation", corlimit = 0.1)))


data <- data %>% 
  mutate(index = map(assocs, ~left_join(.x, lda[[2]], by = c("word" = "term")) %>% 
                       mutate(weight = replace_na(weight, 0),
                              index = cor * weight,
                              n = n()) %>% 
                       summarize(index = sum(index), 
                                 n = mean(n)) %>% 
                       mutate(index = 100*index/sqrt(n)) %>% 
                       select(index)
                     )
         ) %>% unnest(index)

data %>% 
  filter(!is.nan(index)) %>% 
  mutate(year = year(date),
                month = month(date)) %>% 
  group_by(year, month) %>% 
  summarize(index = mean(index, na.rm = TRUE)) %>% 
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>% 
  filter(year > 2000) %>% 
 # left_join(infl_exp, by = "date") %>% 
#  rename(exp = value) %>% 
  gather(key = "var", value ="value", index, #exp
         ) %>% 
  ggplot(aes(x = date, y = value, color = var)) +
  geom_line() +
  scale_y_log10()



#Finding optimal number of topics
library(topicmodels)
perplexity <- matrix(nrow = 20, ncol = 2)
for(k in 2:nrow(perplexity)){
print(paste("Calculating perplexity score for ", k ," topics", sep = ""))  
perplexity[k, 1] <- k
perplexity[k, 2] <- LDA(data_pooled, k = k, method = "Gibbs", control = list(iter = 100, alpha = 0.5, delta = 0.1, seed = 2022)) %>% 
  perplexity(newdata = data_pooled)

}
colnames(perplexity) <- c("k", "perplexity")

perplexity %>% as_tibble %>% 
  drop_na %>% 
  ggplot(aes(x = k, y = perplexity)) +
  geom_point() +
  geom_line()

lda_fin <- LDA(data_pooled, k = 13, 
               method = "Gibbs", control = list(iter = 100, alpha = 0.5, delta = 0.1, seed = 2022))


terms <- terms(lda_fin, k = 100) %>% 
  as_tibble()
