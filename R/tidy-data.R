rm(list = ls())

library(readr)
library(dplyr)
library(stringr)
source('./R/lp-functions.R')

players <- read_delim('./data/players_20.csv', delim = ',', col_names = T) 

players_info <- players %>%
  select(short_name, value_eur, wage_eur, player_positions, club) %>%
  mutate(id = 1:n())

players_stat_string <- players %>%
  select(
    ls, st, rs, lw, lf, cf, rf, rw, lam, cam, ram, lm, lcm, cm, rcm, rm, lwb,
    ldm, cdm, rdm, rwb, lb, lcb, cb, rcb, rb, overall
  ) 

first_score <- players_stat_string %>%
  mutate_all(
    function(x) str_extract(x, '^\\d*') %>%
      as.numeric()
  )

second_score <- players_stat_string %>%
  mutate_all(
    function(x) {
      ifelse(
        str_detect(x, '\\+'), 
        str_extract(x, '\\d*$'), 
        0
      ) %>%
        as.numeric()
    }
  )

players_stat <- as_tibble(first_score + second_score) %>%
  rename(gk = overall) %>%
  mutate(gk = ifelse(players_info$player_positions == 'GK', gk, 0)) %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x))

formations <- read_delim('./data/formations.csv', delim = ';')

formations_matrix <- formations %>%
  select(-c('id', 'formation'))

write_csv2(players_stat, path = './data/players-stat.csv', col_names = T)
write_csv2(players_info, path = './data/players-info.csv', col_names = T)
write_csv2(formations_matrix, path = './data/formations-matrix.csv', 
           col_names = T)
