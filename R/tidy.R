library(NCmisc)
must.use.package('readr')
must.use.package('dplyr')
must.use.package('stringr')
source('./R/utils.R')

players <- read_delim('./data/players_20.csv', delim = ',', col_names = T) 

playersInfo <- players %>%
  select(sofifa_id, short_name, value_eur, wage_eur, player_positions) %>%
  rename(id = sofifa_id)

playersStat <- players %>%
  select(
    overall, ls, st, rs, lw, lf, cf, rf, rw, lam, lm, lcm, cm, rcm, rm, lwb, 
    ldm, cdm, rdm, rwb, lb, lcb, cb, rcb, rb
  ) %>%
  mutate_all(
    function(x) str_extract(x, '^\\d*') %>%
      as.numeric()
  ) %>%
  rename(gk = overall) %>%
  mutate(
    gk = ifelse(playersInfo$player_positions == 'GK', gk, NA)
  ) %>%
  select(sort(names(.)))

players <- bind_cols(playersInfo, playersStat)

save(file = './data/weightMatrix.RData', playersStat)
save(file = './data/players.RData', players)
