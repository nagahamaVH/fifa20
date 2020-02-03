rm(list = ls())

library(dplyr)
library(stringr)
library(readr)
library(ompr)
source('./R/lp-functions.R')

players_info <- read_csv2('./data/players-info.csv')

players_stat <- read_csv2('./data/players-stat.csv') %>%
  as.matrix()

formations <- read_delim('./data/formations.csv', delim = ';')

formations_info <- read_delim('./data/formations-info.csv', delim = ';')

solution <- readRDS('./R/solution.rds')

# Jogadores
solution_players <- solution %>%
  get_solution(dummy_position[i, j]) %>%
  filter(value == 1) %>%
  arrange(i, j)

dream_team <- solution_players %>%
  left_join(players_info, by = c('i' = 'id')) %>%
  mutate(
    score = getFromMatrix(players_stat, i, j),
    position = colnames(players_stat[,j]) %>%
      str_to_upper(),
    mainPosition = ifelse(str_detect(player_positions, position), T, F)
  ) %>%
  left_join(formations_info, by = 'position') %>%
  arrange(order) %>%
  select(short_name, value_eur, wage_eur, score, position, player_positions,
         mainPosition, category, club)

# Formação
solution_formation <- solution %>%
  get_solution(b[k]) %>%
  filter(value == 1) %>%
  left_join(formations, by = c('k' = 'id')) %>%
  select(formation)
