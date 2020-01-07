library(dplyr)
library(stringr)
library(readr)
library(ompr)
source('./R/lp-functions.R')

playersInfo <- read_csv2('./data/players-info.csv')

playersStat <- read_csv2('./data/players-stat.csv') %>%
  as.matrix()

formations <- read_delim('./data/formations.csv', delim = ';')

formationsInfo <- read_delim('./data/formations-info.csv', delim = ';')

solution <- readRDS('./R/solution.rds')

# Jogadores
solutionPlayers <- solution %>%
  get_solution(dummyPosition[i, j]) %>%
  filter(value == 1) %>%
  arrange(i, j)

dreamTeam <- solutionPlayers %>%
  left_join(playersInfo, by = c('i' = 'id')) %>%
  mutate(
    score = getFromMatrix(playersStat, i, j),
    position = colnames(playersStat[,j]) %>%
      str_to_upper(),
    mainPosition = ifelse(str_detect(player_positions, position), T, F)
  ) %>%
  left_join(formationsInfo, by = 'position') %>%
  arrange(order) %>%
  select(short_name, value_eur, wage_eur, score, position, player_positions,
         mainPosition, category, club)

dreamTeam %>%
  summarise(total = sum(value_eur))

# Formação
solutionFormation <- solution %>%
  get_solution(b[k]) %>%
  filter(value == 1)

formations %>%
  filter(id == solutionFormation$k)
