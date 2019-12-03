library(dplyr)
source('./R/utils.R')

load('./solution.RData')

# Indice dos jogadores escolhidos
solutionPlayers <- solution %>%
  get_solution(dummyPosition[i, j]) %>%
  filter(value == 1) %>%
  arrange(i, j)

# Indice da formação escolhida
solutionFormation <- solution %>%
  get_solution(b[k]) %>%
  filter(value == 1)

dreamTeam <- lapply(seq_along(solutionPlayers$i), function(index){
  score <- playersStat[solutionPlayers$i[index], solutionPlayers$j[index]]
  
  position <- names(score)
  
  playerName <- playersInfo$short_name[index]
  
  tibble(playerName, position, score)
}) %>%
  bind_rows()

dreamTeam %>%
  group_by(position) %>%
  count() %>%
  arrange(position)

formations %>%
  filter(id == solutionFormation$k)
