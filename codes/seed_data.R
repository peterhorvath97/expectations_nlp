

seed_data <- tibble(
  topic = c(
    'Exchange Rates',
    'Monetary Policy',
    'Commodity Markets',
    'Financial Markets',
    'Interest Rates',
    'International Economy',
    'Housing',
    'Demand',
    'GDP',
    'Inflation',
    'Credit Market',
    'Labor Market',
    'Fiscal Policy',
    'Supply',
    'Financial Stability',
    'Business Cycle'),
  seedwords = c(
    #Exchange rates
    list(c('dollar')),
    #Monetary Policy
    list(c('statement')),
    #Commodity Markets
    list(c()),
    #Financial Markets
    list(c('asset')),
    #Interest Rates
    list(c('treasuri')),
    #International Economy
    list(c()),
    #Housing
    list(c()),
    #Demand
    list(c()),
    #GDP
    list(c('product')),
    #Inflation
    list(c()),
    #Credit Markets
    list(c('credit')),
    #Labor Market
    list(c('labor')),
    #Fiscal Policy
    list(c()),
    #Supply
    list(c('inventori')),
    #Financial Stability
    list(c()),
    #Business Cycle
    list(c('declin'))
  ))

labels <- seed_data %>% 
  select(theme = topic) %>% 
  mutate(topic = row_number()) %>% 
  select(topic, theme)


