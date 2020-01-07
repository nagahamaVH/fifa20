rm(list = ls())

library(readr)
library(dplyr)
library(stringr)
source('./R/lp-functions.R')

players <- read_delim('./data/players_20.csv', delim = ',', col_names = T) 

playersInfo <- players %>%
  select(short_name, value_eur, wage_eur, player_positions, club) %>%
  mutate(id = 1:n())

playersStat <- players %>%
  select(
    ls, st, rs, lw, lf, cf, rf, rw, lam, cam, ram, lm, lcm, cm, rcm, rm, lwb,
    ldm, cdm, rdm, rwb, lb, lcb, cb, rcb, rb, overall
  ) %>%
  mutate_all(
    function(x) str_extract(x, '^\\d*') %>%
      as.numeric()
  ) %>%
  rename(gk = overall) %>%
  mutate(gk = ifelse(playersInfo$player_positions == 'GK', gk, 0)) %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x))

formations <- read_delim('./data/formations.csv', delim = ';')

formationsMatrix <- formations %>%
  select(-c('id', 'formation'))

write_csv2(playersStat, path = './data/players-stat.csv', col_names = T)
write_csv2(playersInfo, path = './data/players-info.csv', col_names = T)
write_csv2(formationsMatrix, path = './data/formations-matrix.csv', 
           col_names = T)
