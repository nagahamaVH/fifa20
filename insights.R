library(dplyr)
library(stringr)
library(readr)
library(ompr)
source('./R/utils.R')

load('./data/playersInfo.RData')
load('./data/playersStat.RData')
load('./data/solution.RData')

formations <- read_delim('./data/formations.csv', delim = ';')

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
    mainPosition = ifelse(position %in% player_positions, T, F)
  ) %>%
  select(short_name, value_eur, wage_eur, score, position, player_positions,
         mainPosition)

dreamTeam %>%
  summarise(total = sum(value_eur))

# Formação
solutionFormation <- solution %>%
  get_solution(b[k]) %>%
  filter(value == 1)

formations %>%
  filter(id == solutionFormation$k)
