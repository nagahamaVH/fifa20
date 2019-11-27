rm(list = ls())

library(NCmisc)
must.use.package('readr')
must.use.package('dplyr')
must.use.package("ompr", quietly = TRUE)
must.use.package("ompr.roi", quietly = TRUE)
must.use.package("ROI.plugin.symphony", quietly = TRUE)
source('./R/utils.R')

formations <- read_csv2('./data/formations.csv') %>%
  select(-c('id', 'formation')) %>%
  as.matrix()
load('./data/playersInfo.RData')
load('./data/playersStat.RData')

# Informacoes dos jogadores
n <- nrow(playersStat)
m <- ncol(playersStat)

# Informacoes da formacao 
p <- nrow(formations)

model <- MILPModel() %>%
  
  # Variavel indicadora se o i-esimo jogador assume a j-esima posicao
  add_variable(dummyPosition[i, j], i = 1:n, j = 1:m, type = 'binary') %>%
  
  # Variavel indicadora da escolha da formação
  add_variable(C[k], type = "binary", k = 1:p) %>%
  
  # Função objetiva: maximizar a soma do escore do time
  set_objective(sum_expr(colwise(
    getFromMatrix(playersStat, i, j) * dummyPosition[i, j], i = 1:n, 
    j = 1:m)), sense = 'max') %>%
  
  # R: A quantidade total de jogadores na posição deve ser igual a quantidade 
  # especificada na formação escolhida
  # add_constraint(sum_expr(dummyPosition[i, j], i = 1:n) -
  #                  getFromMatrix(formations, i, j) * C[k] == 0, i = 1:p, j = 1:m,
  #                k = 1:p) %>%
  
  # R: O jogador só pode ser designado para uma única posição
  add_constraint(sum_expr(dummyPosition[i, j], j = 1:m) <= 1, i = 1:n) %>%
  
  # R: A formação tática deve ser única
  add_constraint(sum_expr(C[k], k = 1:p) == 1)

result <- solve_model(model, with_ROI(solver = "symphony", 
                                      presolve = T, time_limit = 5 * 60))

# Resultado do modelo
result %>%
  get_solution(dummyPosition[i, j]) %>%
  filter(value == 1)