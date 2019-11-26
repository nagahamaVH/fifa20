library(NCmisc)
must.use.package('readr')
must.use.package('dplyr')
must.use.package('stringr')
source('./R/utils.R')

fifa <- read_delim('./data/data.csv', delim = ',', col_names = T) %>%
  select(-X1)

fifa <- fifa %>%
  select(
    X1,
    ID,
    Name,
    
  )
  mutate(
    Value = StandardizeMoney(Value),
    Wage = StandardizeMoney(Wage)
  )

fifa %>%
  group_by(Position) %>%
  summarise(freq = n()) %>% View()

save(file = './data/fifa.RData', fifa)
